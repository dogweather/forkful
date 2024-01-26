---
title:                "Getting the current date"
date:                  2024-01-20T15:16:14.743709-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Grabbing the current date in Python means fetching the live date from the system it's running on. Programmers do this for logging, timestamps, or whenever today's date is needed for a user interface or a report.

## How to:
Use the `datetime` module. It's straightforward:

```Python
from datetime import datetime

# Get the current date
current_date = datetime.now().date()

# Print it out
print(current_date)
```

Sample output might look like:

```
2023-04-12
```

Note: Output depends on the day you run the code. Obviously.

## Deep Dive
The `datetime` module hasn't changed dramatically over recent Python versions. It's part of Python's standard library – a no-fuss toolset for dealing with dates and times. Alternatives? Sure, there's `time`, but it's cruder. For heavy lifting, the world looks to `dateutil` and `arrow`, but for just today's date? Stick with `datetime`.

Under the hood, `datetime.now()` snags the current moment according to your computer's time settings. To go timezone-aware, you'd use `datetime.now(timezone.utc)`, for example. Historically, dealing with time zones has been a headache, so always consider location and daylight savings if it's vital.

For a quick date without the timestamp – like cooking up a file with today's date in its name – `datetime.now().date()` gives you just that: a date object, containing year, month, and day.

## See Also
- Official Python Docs on `datetime`: https://docs.python.org/3/library/datetime.html
- `arrow` for more complex date/time handling: https://arrow.readthedocs.io
- `dateutil`, because time zones: https://dateutil.readthedocs.io
- Your humble PC's time settings because, well, that's where Python looks first.
