---
title:                "Getting the current date"
html_title:           "Bash recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date means retrieving the current system date and time. Programmers often need the current date for tasks such as timestamping files or tracking when a script was run.

## How to:

```
Bash
# To get the current date in the format "Month Day, Year":
date +"%B %d, %Y"
# Output: May 27, 2021

# To get the current date and time in the format "Day/Month/Year Hour:Minute:Second":
date +"%d/%m/%Y %H:%M:%S"
# Output: 27/05/2021 13:17:42
```

## Deep Dive:

* Historical Context:
Getting the current date has been a common programming task since the early days of computing, when computers were primarily used for data processing and time-sensitive operations.

* Alternatives:
Other programming languages and tools have their own methods for getting the current date. For example, in Python you can use the `datetime` module, and in JavaScript you can use the `Date()` object.

* Implementation Details:
The `date` command in Bash is part of the GNU Core Utilities, which were developed by the Free Software Foundation. It uses the system's clock to retrieve the current date and time, and then formats it according to the given string.

## See Also:

For more information on the `date` command and its options, you can check out the [GNU Core Utilities manual](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html). You can also learn more about working with dates and times in Bash from [Bash Date Command Examples](https://linuxize.com/post/bash-date-command).