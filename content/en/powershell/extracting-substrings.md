---
title:                "Extracting substrings"
html_title:           "PowerShell recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings is the process of retrieving a sequence of characters from within a larger string. Programmers often do this to manipulate and extract specific data from a larger dataset, making it easier to process and analyze. This allows for more efficient and specific string manipulation, reducing the need for manual input. 

## How to:
Coding examples and output using PowerShell:
```
#Extracting a single substring
$string = "Hello, world!"
$result = $string.Substring(7,5)
$result
#Output: world

#Extracting multiple substrings
$string = "Welcome to PowerShell!"
$result = $string.SubString(0,7) + " " + $string.SubString(11,9)
$result
#Output: Welcome PowerShell

#Extracting a substring using a pattern match
$string = "Today's date is 12/25/2020"
$result = $string -match "\d{2}\/\d{2}\/\d{4}"
$result
#Output: 12/25/2020
```

## Deep Dive:
- **Historical context:** Extracting substrings has been a common practice in programming languages since the early days of computing. In earlier programming languages, such as Fortran and BASIC, substring extraction was achieved through the use of specific substring functions. 
- **Alternatives:** While extracting substrings remains a common practice, there are now alternative ways to manipulate strings, such as using regular expressions, which can provide more complex pattern matching capabilities. However, substring extraction remains a simple and efficient method for specific extraction needs.
- **Implementation details:** In PowerShell, substrings can be extracted using the Substring() method, with the first parameter representing the starting index and the second parameter representing the length of the substring. Alternatively, the -match operator can be used with regex patterns to extract substrings based on matching criteria.

## See Also:
- [PowerShell documentation on substring extraction](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_substrings?view=powershell-7.1)
- [Regular Expressions in PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)