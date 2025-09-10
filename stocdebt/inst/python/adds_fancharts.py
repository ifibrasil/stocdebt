import pandas as pd
import numpy as np
import xlsxwriter

def adds_fancharts(excel_input,excel_output,vars_fanchart,percentile_labels,the_date_format,chart_with_title,x_axis_label,y_axis,color):

    """Creates a copy of file 'excel_input', but adding fan charts.

    Usage
    ----------

    adds_fancharts(
        excel_input: 'str',
        excel_output: 'str',
        vars_fanchart: 'list',
        percentile_labels: 'bool',
        the_date_format: 'str',
        chart_with_title: 'bool',
        x_axis_label: 'str',
        y_axis: 'dict',
        color: 'str'
        )

    Parameters
    ----------

    excel_input: path (either relative or absolute) to the OLD xlsx file produced by function 'export_to_excel()' of the R package 'stocdebt'.
    
    excel_output: path (either relative or absolute) to the NEW xlsx file to be created, which will contain the fan charts. If equal to 'excel_input', then 'excel_input' will be overwritten.
    
    vars_fanchart: a list with the names of the variables for which fan charts should be created.

    percentile_labels: should labels be written for the percentiles on the fan chart?

    the_date_format: string in the form 'dd/mm/yyyy'. For example, to see only years, set 'yyyy'.

    chart_with_title: should the fan charts have titles (they will be the variable names)?

    x_axis_label: string to be written as the x axis label.
    
    y_axis: dictionary of lists. Each dictionary KEY must be named after one of the varibles. Each dictionary VALUE must be a list with numeric lower and upper bounds for the y axis of the fan chart. See the example.

    color: RGB color of the fan.

    Example
    ----------

    >>> adds_fancharts(
    ...     excel_input  = './my_files/results.xlsx',
    ...     excel_output = './other_files/results_with_fancharts.xlsx',
    ...     vars_fanchart = ['debt','primary balance'],
    ...     percentile_labels = True,
    ...     the_date_format = 'yyyy',
    ...     chart_with_title = True,
    ...     x_axis_label = 'Period',
    ...     y_axis = {'debt':[0,100],'primary balance':[-5,5]},
    ...     color = '#005d89'
    ...     )

    Returns
    ----------

    No python object (silent return). The only consequence is the creation of a new xlsx file on the path informed in argument 'excel_output'.

    """

    # Creates new excel file
    new_wb = xlsxwriter.Workbook(excel_output)

    # Creates an index (summary) worksheet as the first sheet of the file
    index_ws = new_wb.add_worksheet(name="index")

    # Writes all sheets using tables from the old excel file
    old_wb = pd.ExcelFile(path_or_buffer=excel_input)

    # Initializes dictionaries to store worksheets links and descriptions (to be used in the index sheet)
    worksheet_links = {}
    worksheet_variable_names = {}
    worksheet_descriptions = {}

    for sheet in old_wb.sheet_names:

        # Creates and stores the link to the sheet
        # ----------------------------------------------------
        worksheet_links[sheet] = f"'{sheet}'!A1"

        # Imports table from old excel
        # ----------------------------------------------------
        table = old_wb.parse(header = None,sheet_name = sheet)
        worksheet_variable_names[sheet] = table.iloc[1][0] # Stores the name of the variable whose values are in this worksheet
        worksheet_descriptions[sheet] = table.iloc[0][0] # Stores the description of the table which is in this worksheet

        # Creates new worksheet in the new excel 
        # ----------------------------------------------------
        n = new_wb.add_worksheet(name=sheet)

        # Writes table into new excel (after replacing occurrences of 'Unnamed' and 'nan' with '')
        # ----------------------------------------------------
        date_format = new_wb.add_format({'num_format':the_date_format})
        n.set_column('A:A', 11)

        for the_column in range(table.shape[1]):
            for the_line in range(table.shape[0]):

                if pd.isna(table.iloc[the_line][the_column]):
                    n.write(the_line,the_column,'')
                elif the_line > 2 and the_column == 0:
                    n.write(the_line,the_column,table.iloc[the_line][the_column],date_format)
                else:
                    n.write(the_line,the_column,table.iloc[the_line][the_column])

        # Creating the chart sheet
        # ----------------------------------------------------

        # Preparing vars_fanchart object to avoid error (when vars_fanchart is a single string)
        if isinstance(vars_fanchart,str) == True:
            vars_fanchart = [vars_fanchart]

        for the_var in vars_fanchart:
            if sheet == "diff_" + the_var:
                # Creates the sheet that will receive the fanchart
                # -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
                chartsheet = new_wb.add_worksheet(name="fc_"+the_var)
                # Stores the variable name and the description of this worksheet
                # -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
                worksheet_variable_names["fc_"+the_var] = table.iloc[1][0]
                worksheet_descriptions["fc_"+the_var] = "Fan chart"
                # Creates and stores the link to the sheet
                # -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
                worksheet_links["fc_"+the_var] = "'fc_" + the_var + "'!A1"
                # Plots the baseline scenario (line chart)
                # -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
                fanchart = new_wb.add_chart({'type':'line'})
                fanchart.add_series({
                    'categories': ["diff_" + the_var, 3, 0, table.shape[0]-1, 0],
                    'values': ["diff_" + the_var, 3, 1, table.shape[0]-1, 1],
                    'line': {'color':color}
                })
                # Creates the ribbons (for "stacked areas" chart)
                # -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
                ribbons = new_wb.add_chart({'type':'area', 'subtype':'stacked'})
                data = table[2:].reset_index(drop=True)
                # Defines transparency of ribbons
                quantile_columns = [*range(2,table.shape[1])]
                quantiles = data.iloc[0][quantile_columns]
                quantiles = quantiles.tolist()

                quantile_columns = {quantiles[i]:quantile_columns[i] for i in range(len(quantiles))} # (Will be useful later)

                transparencies = {quantiles[0]:100} # Sets transparency of first ribbon to 100%, i.e. the ribbon will be intentionally not visible

                # (If even number of quantiles)
                if len(quantiles) % 2 == 0:
                    quantiles_ex1 = quantiles[1:] # 'ex1' means 'except the first'
                    first_quantiles = quantiles_ex1[0 : len(quantiles_ex1)//2 + 1]
                    last_quantiles = quantiles_ex1[len(quantiles_ex1)//2 + 1 : len(quantiles_ex1)]
                    # (Note: intentionally hard-coded '85' and '55' below -- just an arbitrary choice of acceptable limits for transparency)
                    temp = np.linspace(start=85,stop=55,num=len(first_quantiles))
                    temp = temp.tolist()
                    temp = [int(i) for i in temp]
                    temp = temp + list(reversed(temp))[1:]
                    temp = {quantiles_ex1[i] : temp[i] for i in range(len(temp))}
                    transparencies.update(temp)
                # (If odd number of quantiles)
                else:
                    quantiles_ex1 = quantiles[1:] # 'ex1' means 'except the first'
                    first_quantiles = quantiles_ex1[0 : len(quantiles_ex1)//2]
                    last_quantiles = quantiles_ex1[len(quantiles_ex1)//2 : len(quantiles_ex1)]
                    # (Note: intentionally hard-coded '85' and '55' below -- just an arbitrary choice of acceptable limits for transparency)
                    temp = np.linspace(start=85,stop=55,num=len(first_quantiles))
                    temp = temp.tolist()
                    temp = [int(i) for i in temp]
                    temp = temp + list(reversed(temp))
                    temp = {quantiles_ex1[i] : temp[i] for i in range(len(temp))}
                    transparencies.update(temp)

                if percentile_labels == True:
                    for q in quantiles:
                        custom_labels = [{"delete":True}]*(table.shape[0]-1-3) # (Last row position) - (first row position)
                        custom_labels.append({'value':q})
                        ribbons.add_series({
                            'categories': ["diff_" + the_var, 3, 0, table.shape[0]-1, 0],
                            'values': ["diff_" + the_var, 3, quantile_columns[q], table.shape[0]-1, quantile_columns[q]],
                            'data_labels': {'custom': custom_labels},
                            'fill': {'color':color,'transparency':transparencies[q]}
                        })
                else:
                    for q in quantiles:
                        ribbons.add_series({
                            'categories': ["diff_" + the_var, 3, 0, table.shape[0]-1, 0],
                            'values': ["diff_" + the_var, 3, quantile_columns[q], table.shape[0]-1, quantile_columns[q]],
                            'fill': {'color':color,'transparency':transparencies[q]}
                        })
                # Combines the baseline and the ribbons, to form the fanchart
                fanchart.combine(ribbons)
                if chart_with_title == True:
                    fanchart.set_title({'name': table.iloc[1][0]})
                fanchart.set_x_axis({'name': x_axis_label,'label_position': 'low'})
                fanchart.set_legend({'none': True})
                if table.iloc[1][0] in y_axis:
                    fanchart.set_y_axis({
                        'min': y_axis[table.iloc[1][0]][0],
                        'max': y_axis[table.iloc[1][0]][1]
                        })
                fanchart.set_chartarea({'border': {'none': True}})
                # Places the fanchart into the appropriate worksheet in the excel file
                chartsheet.insert_chart(row=2,col=1,chart=fanchart)
                chartsheet.write(0,0,"Fan chart for variable: "+ table.iloc[1][0])

    # Writes the content of the index sheet

    bold_format = new_wb.add_format({'bold': True})
    bold_large_format = new_wb.add_format({'bold': True, 'font_size': 14})

    index_ws.write(2,2,"File index",bold_large_format)

    index_ws.write(4,2,"Sheet",bold_format)
    index_ws.set_column('C:C', 25)

    index_ws.write(4,3,"Variable",bold_format)
    index_ws.set_column('D:D', 40)

    index_ws.write(4,4,"Description",bold_format)
    index_ws.set_column('E:E', 70)

    k = 4 # Initializes row number indicator
    for i in worksheet_descriptions.keys():
        k += 1
        # Writes sheet name and link to the sheet
        try:
            index_ws.write_url(k,2,"internal:"+worksheet_links[i], string = i)
        except: # This is for cases when the sheet is a "chartsheet" (which does not support link to it)
            index_ws.write(k,2,i)
        # Writes the name of the variable in the sheet
        if i == "baseline" or i == "stochastic":
            index_ws.write(k,3,"all")
        else:
            index_ws.write(k,3,worksheet_variable_names[i])
        # Writes the description of the sheet
        index_ws.write(k,4,worksheet_descriptions[i])

    new_wb.close()
