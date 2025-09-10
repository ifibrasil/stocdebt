.onLoad <- function(libname, pkgname) {

	the_path <- base::Sys.which(base::c("python","python3"))

	no_python_message <- base::paste(
			"	Warning: R has not found a Python installation on this computer.",
			"	  - If you do have Python installed, you can manually inform its location to R using:",
			"",
			"		> reticulate::use_python(\"/path/to/python\")",
			"",
			"	  - If you do not intend to export CHARTS, you can proceed and use the package, because all other features will work appropriately.",
			sep = "\n"
	)
  
	python_is_stub_message <- base::paste(
			"	Warning: R looked for a Python installation, but the Python \".exe\" file found appears to be a Windows \"stub\", not a real Python.",
			"	  - To be able to export charts using package \"stocdebt\", you will have to effectivey download and install Python.",
			"	  - If you are sure to have Python installed, you can manually inform its location to R using:",
			"",
			"		> reticulate::use_python(\"/path/to/python\")",
			"",
			"	  - Using the command above solves the problem TEMPORARILY, only during the current session.",
			"	  - If you want a PERMANENT solution, manually add \"/path/to/python\" to Windows PATH (Windows environment variable)",
			"	  - If you do not intend to export CHARTS, you can proceed and use the package, because all other features will work appropriately.",
			sep = "\n"
	)
 

 
	# If there is no Python, do this...
	# _____________________________________________________________________________________________

	if (base::all(the_path == "")) {base::packageStartupMessage(no_python_message)



	# ...but if Python3 is apparently installed, test it and use it, even if Python (not Python3) is installed as well...
	# _____________________________________________________________________________________________

	} else if (the_path["python3"] != "") {

		# If the OS is Windows...
		if (base::Sys.info()["sysname"] == "Windows"){

			# tests if file .exe is a stub or a real executable of Python...
			temp <- base::suppressWarnings(base::file.info(the_path["python3"]))
			temp <- temp[1,"exe"]

			if (temp == "no"){
				# If .exe is a stub, warns the user
				base::packageStartupMessage(python_is_stub_message)
			} else {
				# If .exe is a real executable, uses this Python installation
				reticulate::use_python(the_path["python3"], required = FALSE)
				base::packageStartupMessage("	Using Python at: ", the_path["python3"])
			}

		# but if the OS is Linux...
		} else if (base::Sys.info()["sysname"] == "Linux"){

			# Uses this Python installation
			reticulate::use_python(the_path["python3"], required = FALSE)
			base::packageStartupMessage("	Using Python at: ", the_path["python3"])
		}



	# ...but if only Python is apparently installed (not Python3), test it and use it.
	# _____________________________________________________________________________________________

	} else {

		# If the OS is Windows...
		if (base::Sys.info()["sysname"] == "Windows"){

			# tests if file .exe is a stub or a real executable of Python...
			temp <- base::suppressWarnings(base::file.info(the_path["python"]))
			temp <- temp[1,"exe"]

			if (temp == "no"){
				# If .exe is a stub, warns the user
				base::packageStartupMessage(python_is_stub_message)
			} else {
				# If .exe is a real executable, uses this Python installation
				reticulate::use_python(the_path["python"], required = FALSE)
				base::packageStartupMessage("	Using Python at: ", the_path["python"])
			}

		# but if the OS is Linux...
		} else if (base::Sys.info()["sysname"] == "Linux"){

			# Uses this Python installation
			reticulate::use_python(the_path["python"], required = FALSE)
			base::packageStartupMessage("	Using Python at: ", the_path["python"])
		}
	}

}
