---
title:    "Bash recipe: Converting a date into a string"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why 

Converting a date into a string is a common task in Bash programming. It allows you to manipulate and format dates in a more user-friendly way, making them easier to read and understand. This can be useful for creating reports, file naming conventions, or any other situation where a human-readable date is needed. 

## How To 

Converting a date into a string can be done using the `date` command in Bash. You can specify the format of the date by using the `+` flag followed by a string representing the desired format. For example, to convert today's date into a string with the format "Month, Day Year", you can use the following command: 

```Bash 
date +"%B, %d %Y" 
```

This will output something like "May, 30 2021". 

If you want to include the time as well, you can use the format "Month, Day Year Hour:Minute: Second": 

```Bash 
date +"%B, %d %Y %H:%M:%S" 
```

Which will output something like "May, 30 2021 12:30:15". 

You can also use a combination of different format string options to get a customized output. For example, if you want the date to be in the format "Day-Month-Year", you can use: 

```Bash 
date +"%d-%m-%Y" 
```

Which will output "30-05-2021". 

## Deep Dive 

The `date` command offers a variety of options for formatting dates. Here are some commonly used format string options: 

- `%a` - abbreviated weekday name (e.g. Mon, Tue, Wed)
- `%A` - full weekday name (e.g. Monday, Tuesday, Wednesday) 
- `%b` - abbreviated month name (e.g. Jan, Feb, Mar) 
- `%B` - full month name (e.g. January, February, March) 
- `%d` - day of the month (e.g. 01, 02, 30) 
- `%Y` - year (e.g. 2021) 
- `%H` - hour (00-23) 
- `%M` - minute (00-59) 
- `%S` - second (00-59) 

For a full list of all the available options, you can check the `date` command's manual page by typing `man date` in your terminal. 

## See Also 

- [Bash date command](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html) 
- [Bash printf command](https://www.gnu.org/software/coreutils/manual/html_node/printf-invocation.html) 
- [Bash string manipulation](https://www.tldp.org/LDP/abs/html/string-manipulation.html) 

Converting a date into a string can save you time and make your code more readable. Give it a try and see how it can benefit your Bash programming!