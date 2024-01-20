---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string means to extract and interpret the constituent parts of a date (like day, month, and year) from a text string. Programmers do this to use, manipulate, or compare dates within their scripts.

## How To:

Let's discuss the most common option for date parsing in Bash using the `date` command.

```Bash
# Define date string
date_string="2021-07-07 16:40:30"
# Parse date string to a date
parsed_date=$(date -d "$date_string" +"%Y-%m-%d %T")
echo $parsed_date
```

Output:

```Bash
2021-07-07 16:40:30
```

Here, the `-d` option makes `date` interpret the input string, and the `+"%Y-%m-%d %T"` part formats the output.

## Deep Dive:

Historically, date parsing in Unix-like systems was a bit of a pain due to varying date formats across geographical locations. The current `date` command in GNU Coreutils, used in Bash, became standardized for more consistency.

The `date` command is not the only way to parse dates. If you want to get creative, use other Unix utilities like `awk`, `sed`, or Python with its `datetime` module to parse dates.

After parsing a date from a string, Bash holds it as a UNIX timestamp, the number of seconds since 1970-01-01 00:00:00 UTC. When required, it formats this timestamp to human-readable forms.

## See Also:

1. GNU Coreutils `date` manual: [https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html) 
2. Further reading on the `awk` command: [https://www.gnu.org/software/gawk/manual/](https://www.gnu.org/software/gawk/manual/)
3. Python `datetime` processing: [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)