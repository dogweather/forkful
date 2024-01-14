---
title:                "Bash recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why 
As a programmer, it's important to have a solid understanding of how to work with command line arguments in Bash. By learning this skill, you can create more dynamic and user-friendly scripts, making your code more versatile and efficient. Plus, knowing how to read command line arguments can save you time and effort when executing your scripts.

## How To 
Let's dive into the basics of reading command line arguments in Bash. The first step is to declare your variables and use the "$" symbol before the number in order to reference the argument position. For example, $1 refers to the first argument, $2 refers to the second, and so on.

```Bash
# declaring variables
first_name=$1
last_name=$2

echo "Welcome, $first_name $last_name!" 
```
This code will take two arguments from the command line (in the format of First Name Last Name) and output a personalized greeting. If no arguments are given, the script will still run, but the output will be "Welcome, !". You can also use the "$@" symbol to refer to all the arguments given in a single line.

Another useful command is "shift", which allows you to move through the arguments in a loop. This can be helpful when working with multiple arguments and their values. Let's see an example:

```Bash
while [[ "$#" -gt 0 ]]; do
  echo "$1"
  shift
done
```
This will loop through each argument in the command line and print them out on separate lines. 

Lastly, if you want to prompt the user for input instead of taking arguments from the command line, you can use the "read" command. It will wait for the user to input their desired value and assign it to the declared variable. 

```Bash
echo "What is your favorite color?"
read color
echo "Your favorite color is $color!"
```

## Deep Dive 
Now that you have a basic understanding of how to read command line arguments, let's take a deeper look at some additional features that can enhance your scripts. 

One important aspect to consider is how to handle errors when no arguments are given. You can use the "if" statement to check for the presence of arguments and give an error message if none are found.

```Bash
if [ $# -eq 0 ]; then
  echo "Error: No arguments given."
  exit 1
fi
```
Additionally, you can use flags and options when using command line arguments. This allows you to give users more control over the behavior of your script. Flags are single characters preceded by a hyphen, while options are longer words preceded by two hyphens. You can use the "getopts" command to parse flags and options, making your scripts more user-friendly.

```Bash
while getopts "hd:o:" opt; do
  case $opt in
    h) # help flag
      echo "Usage: your_script_name [OPTION]... [ARGUMENT]..."
      echo " -h: display help menu"
      echo " -d: set directory"
      echo " -o: specify output file name"
      echo "For more info, type 'man your_script_name'"
      exit 1;;
    d) # directory option
      cd $OPTARG
      ;;
    o) # output option
      output_name=$OPTARG
      ;;
    *) # invalid option
      echo "Invalid option. For more info, type 'man your_script_name'"
      exit 1;;
  esac
done
```

## See Also
For more information on working with command line arguments in Bash, check out the following resources: 

- [Bash Command Line Arguments](https://linuxhint.com/bash_command_line_arguments/)
- [Working with Arguments in Bash](https://ryanstutorials.net/bash-scripting-tutorial/bash-arguments.php)
- [Command Line Arguments in Bash](https://scriptingosx.com/2017/04/the-best-way-to-parse-command-line-arguments/)