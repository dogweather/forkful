---
title:    "Bash recipe: Getting the current date"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why 
Have you ever needed to know the current date while working on a project in Bash programming? Maybe you want to include the date in a file name or use it for version control. Whatever the case may be, knowing how to retrieve the current date in Bash can be a useful skill to have.

## How To 
To get the current date in Bash, we can use the `date` command. The general syntax for this command is as follows: 
```Bash 
date [OPTION]... [+FORMAT] 
```
Some common options include `-d` for specifying a specific date and `-u` for displaying the date in UTC time. The `+FORMAT` option allows us to customize the output of the date. For example, if we want to display the date in the format of "Month Day, Year", we can use the following command: 
```Bash 
date +"%B %d, %Y" 
```
This will output something like "April 20, 2020". Alternatively, we can use the `+"%D"` format to display the date in the format of "MM/DD/YY". The possibilities are endless with the formatting options for the `date` command.

For more advanced users, we can also combine the `date` command with other Bash commands to manipulate the date in different ways. For example, we can use command substitution to store the current date in a variable for later use. Consider the following example: 
```Bash 
current_date=$(date +"%m/%d/%y") 
echo "The current date is $current_date" 
``` 
This will output something like "The current date is 04/20/20". 

## Deep Dive 
The `date` command is a part of the GNU Core Utilities package, meaning that it is not specific to Bash and can be used on any Unix-based system. It uses the system's time and date settings, so if those settings are incorrect, the `date` command may produce incorrect results.

Additionally, the `date` command relies on the system's clock, which can sometimes be affected by time synchronization issues. If the system's clock is off, the output of the `date` command will also be off. It's important to regularly synchronize the system's time to avoid any discrepancies.

## See Also
- [GNU Core Utilities](https://www.gnu.org/software/coreutils/)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/)
- [Bash Command Substitution](https://www.gnu.org/software/bash/manual/html_node/Command-Substitution.html)