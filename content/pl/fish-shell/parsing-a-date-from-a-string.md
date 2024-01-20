---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:35:54.519991-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Parsing a date from a string means extracting date information from text. Programmers do it to manipulate dates, compare them, or convert formats.

## How to: (Jak to zrobić:)
In Fish Shell, you can use `date` command with custom formats for parsing.

```Fish Shell
# Parse a date string into a specific format
set date_str "2023-03-14 15:09:26"
date -d $date_str "+%Y-%m-%d"

# Sample Output: 2023-03-14
```

```Fish Shell
# Convert a date string to epoch time
date -d "2023-03-14 15:09:26" "+%s"

# Sample Output: 1678812566
```

## Deep Dive (Dogłębna analiza)
Parsing dates from strings wasn't always straightforward. Historically, Unix systems used different tools, with `date` being a standard later on. Alternatives like `strptime` exist in programming languages like Python or C, but for Fish Shell, `date` is the go-to tool. It handles multiple date formats and can convert to both human-readable forms and epoch time. Implementation depends on clearly defining the expected date string format and using `date` accordingly.

## See Also (Zobacz również)
- Fish Shell documentation for date: https://fishshell.com/docs/current/cmds/date.html
- GNU Coreutils 'date': https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation
- Comprehensive date format specifier guide: https://en.wikipedia.org/wiki/Date_(Unix)#Formatting