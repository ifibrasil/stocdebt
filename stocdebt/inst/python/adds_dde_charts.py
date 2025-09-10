import pandas as pd
import numpy as np
import xlsxwriter

def adds_dde_charts(excel_input,excel_output,d,stock_flow_adjustments,the_date_format,chart_with_title,x_axis_label):

    """Creates a copy of file 'excel_input', but adding line charts.

    Usage
    ----------

    adds_dde_charts(
        excel_input: 'str',
        excel_output: 'str',
        d: 'str',
        stock_flow_adjustments: 'str',
        the_date_format: 'str',
        chart_with_title: 'bool',
        x_axis_label: 'str'
        )

    Parameters
    ----------

    excel_input: path (either relative or absolute) to the OLD xlsx file produced by function 'export_to_excel()' of the R package 'stocdebt'.
    
    excel_output: path (either relative or absolute) to the NEW xlsx file to be created, which will contain charts. If equal to 'excel_input', then 'excel_input' will be overwritten.

    d: name of the debt variable.

    stock_flow_adjustments: name of the stock-flow adjustments variable.
    
    the_date_format: string in the form 'dd/mm/yyyy'. For example, to see only years, set 'yyyy'.

    chart_with_title: should the charts have titles (they will be the variable names)?

    x_axis_label: string to be written as the x axis label.

    Example
    ----------

    >>> adds_dde_charts(
    ...     excel_input  = './my_files/results.xlsx',
    ...     excel_output = './other_files/results_with_charts.xlsx',
    ...     d = 'debt',
    ...     stock_flow_adjustments = 'adjustments',
    ...     the_date_format = 'yyyy',
    ...     chart_with_title = True,
    ...     x_axis_label = 'Period'
    ...     )

    Returns
    ----------

    No python object (silent return). The only consequence is the creation of a new xlsx file, on the path informed in argument 'excel_output'.

    """

    # Creates new excel file
    new_wb = xlsxwriter.Workbook(excel_output)

    # Writes all sheets using tables from the old excel file
    old_wb = pd.ExcelFile(path_or_buffer=excel_input)




    # Imports table from old excel
    # ___________________________________________________________________
    # ___________________________________________________________________

    table = old_wb.parse(header = None,sheet_name = "data")

    # Creates new worksheet in the new excel 
    n = new_wb.add_worksheet(name="data")

    # Writes table into new excel (after replacing occurrences of 'Unnamed' and 'nan' with '')
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




    # Creating the charts
    # ___________________________________________________________________
    # ___________________________________________________________________

    # Creates chart 1 of 4 (real debt VERSUS debt simulated by the dynamics equation: FIXED initial condition)
    # ---------------------------------

    real_x_sim = new_wb.add_chart({'type':'line'})

    # Adds a line for realized debt
    the_column = table.iloc[2].eq(d).idxmax()
    real_x_sim.add_series({
        'categories': ["data", 3, 0, table.shape[0]-1, 0],
        'values': ["data", 3, the_column, table.shape[0]-1, the_column],
        'name': 'Realized debt',
        'line': {'color':'#005d89'}
    })
    # Adds a line for simulated debt (version with fixed init. cond.)
    the_column = table.iloc[2].eq(d+" (simulated, fixed init. cond.)").idxmax()
    real_x_sim.add_series({
        'categories': ["data", 3, 0, table.shape[0]-1, 0],
        'values': ["data", 3, the_column, table.shape[0]-1, the_column],
        'name': 'Simulated debt (fixed init. cond)',
        'line': {'color':'#bd534b'}
    })
    # Formats the chart
    if chart_with_title == True:
        real_x_sim.set_title({'name': "Realized and simulated debt, fixed initial condition (% of GDP)"})
    real_x_sim.set_x_axis({'name': x_axis_label,'label_position': 'low'})
    real_x_sim.set_legend({'position': 'bottom'})
    real_x_sim.set_chartarea({'border': {'none': True}})
    # Places the chart in the appropriate worksheet in the excel file
    the_column = table.shape[1]+2
    n.write(3,the_column,"Comparison between realized and simulated debt")
    n.write(4,the_column,"Debt simulated with a fixed initial condition")
    n.insert_chart(row=5,col=the_column,chart=real_x_sim)

    # Creates chart 2 of 4 (real debt VERSUS debt simulated by the dynamics equation: MOVING initial condition)
    # ---------------------------------

    real_x_sim_moving_init_cond = new_wb.add_chart({'type':'line'})

    # Adds a line for realized debt
    the_column = table.iloc[2].eq(d).idxmax()
    real_x_sim_moving_init_cond.add_series({
        'categories': ["data", 3, 0, table.shape[0]-1, 0],
        'values': ["data", 3, the_column, table.shape[0]-1, the_column],
        'name': 'Realized debt',
        'line': {'color':'#005d89'}
    })
    # Adds a line for simulated debt (version with moving init. cond.)
    the_column = table.iloc[2].eq(d+" (simulated, moving init. cond.)").idxmax()
    real_x_sim_moving_init_cond.add_series({
        'categories': ["data", 3, 0, table.shape[0]-1, 0],
        'values': ["data", 3, the_column, table.shape[0]-1, the_column],
        'name': 'Simulated debt (moving init. cond.)',
        'line': {'color':'#bd534b'}
    })
    # Formats the chart
    if chart_with_title == True:
        real_x_sim_moving_init_cond.set_title({'name': "Realized and simulated debt, moving initial condition (% of GDP)"})
    real_x_sim_moving_init_cond.set_x_axis({'name': x_axis_label,'label_position': 'low'})
    real_x_sim_moving_init_cond.set_legend({'position': 'bottom'})
    real_x_sim_moving_init_cond.set_chartarea({'border': {'none': True}})
    # Places the chart in the appropriate worksheet in the excel file
    the_column = table.shape[1]+2
    n.write(21,the_column,"Comparison between realized and simulated debt")
    n.write(22,the_column,"Debt simulated with a moving initial condition")
    n.insert_chart(row=23,col=the_column,chart=real_x_sim_moving_init_cond)

    # Creates chart 3 of 4 (debt difference [real - simulated, fixed init. cond.] VERSUS stock-flow adjustments)
    # ---------------------------------

    if stock_flow_adjustments is not None:

        diff_x_sfa = new_wb.add_chart({'type':'line'})

        # Adds a line for debt difference (real - simulated, fixed init. cond.)
        the_column = table.iloc[2].eq(d+" (real - simulated, fixed init. cond.)").idxmax()
        diff_x_sfa.add_series({
            'categories': ["data", 3, 0, table.shape[0]-1, 0],
            'values': ["data", 3, the_column, table.shape[0]-1, the_column],
            'name': 'Debt difference (real - simulated, fixed init. cond.)',
            'line': {'color':'#005d89'}
        })
        # Adds a line for stock-flow adjustments
        the_column = table.iloc[2].eq(stock_flow_adjustments).idxmax()
        diff_x_sfa.add_series({
            'categories': ["data", 3, 0, table.shape[0]-1, 0],
            'values': ["data", 3, the_column, table.shape[0]-1, the_column],
            'name': 'Realized stock-flow adjustments',
            'line': {'color':'#bd534b'}
        })
        # Formats the chart
        if chart_with_title == True:
            diff_x_sfa.set_title({'name': "Debt difference (real - simulated, fixed init. cond.) and stock-flow adjustments (p.p. of GDP)"})
        diff_x_sfa.set_x_axis({'name': x_axis_label,'label_position': 'low'})
        diff_x_sfa.set_legend({'position': 'bottom'})
        diff_x_sfa.set_chartarea({'border': {'none': True}})
        # Places the chart in the appropriate worksheet in the excel file
        the_column = table.shape[1]+2
        n.write(39,the_column,"Comparison between debt difference and stock-flow adjustments")
        n.write(40,the_column,"Debt simulated with a fixed initial condition")
        n.insert_chart(row=41,col=the_column,chart=diff_x_sfa)

    # Creates chart 4 of 4 (debt difference [real - simulated, moving init. cond.] VERSUS stock-flow adjustments)
    # ---------------------------------

    if stock_flow_adjustments is not None:

        diff_moving_init_cond_x_sfa = new_wb.add_chart({'type':'line'})

        # Adds a line for debt difference (real - simulated, moving init. cond.)
        the_column = table.iloc[2].eq(d+" (real - simulated, moving init. cond.)").idxmax()
        diff_moving_init_cond_x_sfa.add_series({
            'categories': ["data", 3, 0, table.shape[0]-1, 0],
            'values': ["data", 3, the_column, table.shape[0]-1, the_column],
            'name': 'Debt difference (real - simulated, moving init. cond)',
            'line': {'color':'#005d89'}
        })
        # Adds a line for stock-flow adjustments
        the_column = table.iloc[2].eq(stock_flow_adjustments).idxmax()
        diff_moving_init_cond_x_sfa.add_series({
            'categories': ["data", 3, 0, table.shape[0]-1, 0],
            'values': ["data", 3, the_column, table.shape[0]-1, the_column],
            'name': 'Realized stock-flow adjustments',
            'line': {'color':'#bd534b'}
        })
        # Formats the chart
        if chart_with_title == True:
            diff_moving_init_cond_x_sfa.set_title({'name': "Debt difference (real - simulated, moving init. cond.) and stock-flow adjustments (p.p. of GDP)"})
        diff_moving_init_cond_x_sfa.set_x_axis({'name': x_axis_label,'label_position': 'low'})
        diff_moving_init_cond_x_sfa.set_legend({'position': 'bottom'})
        diff_moving_init_cond_x_sfa.set_chartarea({'border': {'none': True}})
        # Places the chart in the appropriate worksheet in the excel file
        the_column = table.shape[1]+2
        n.write(57,the_column,"Comparison between debt difference and stock-flow adjustments")
        n.write(58,the_column,"Debt simulated with a moving initial condition")
        n.insert_chart(row=59,col=the_column,chart=diff_moving_init_cond_x_sfa)

    new_wb.close()
