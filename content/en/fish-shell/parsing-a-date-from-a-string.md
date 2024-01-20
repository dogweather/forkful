---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:36:14.617111-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string involves reading date information that's formatted as text and converting it to a date data structure the program can understand. Programmers do this to manipulate and work with dates—think analytics, scheduling, or simply displaying them in a different format.

## How to:

```Fish Shell
# Basic date parsing using `strptime` function
set date_string "2023-04-15"
set -l format "%Y-%m-%d"
set -l parsed_date (string tolower (date -u --date=$date_string +"$format"))

echo $parsed_date # Outputs: 2023-04-15
```

```Fish Shell
# Handling multiple date formats using a switch
set date_string1 "15-04-2023"
set date_string2 "April 15, 2023"

function parse_date -a date_string
    switch $date_string
        case "*-*-*"
            date --date=$date_string +%Y-%m-%d
        case "* *, *"
            date --date=$date_string +%Y-%m-%d
    end
end

echo (parse_date $date_string1) # Outputs: 2023-04-15
echo (parse_date $date_string2) # Outputs: 2023-04-15
```

## Deep Dive

Fish Shell doesn't have built-in date parsing functions like some other languages. Instead, it leans on external utilities like `date`. The `date` command is versatile, and with help from `strptime` (string parse time), which is a standard C library function, it can handle many date formats.

Before `date` and `strptime`, programmers wrote custom parsers—often buggy and complex. Now, utilities handle the quirks of time zones and leap years, sparing us headaches.

Alternatives? Sure, scripting languages like Python have robust date-time libraries like `datetime`. But Fish, being a 'shell', prefers lightweight, command-line programs for a job like this.

In our examples, we used `switch` to choose the date format for `date` to parse. It's clean and extendable. Want more formats? Add more `case` blocks.

Why `string tolower` in the first example? It's about consistency, ensuring the format string and output are uniformly lowercase. A small touch, but it illustrates Fish's preference for simple string operations.

## See Also

- The `date` man page: `man date`
- Fish Shell's string manipulation documentation: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- General date command usage examples: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)