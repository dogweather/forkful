---
title:    "Gleam recipe: Getting the current date"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why
As developers, we often need to track and manipulate dates in our code. Whether it's for creating time-sensitive logic or displaying the current date on a website, being able to access the current date is a useful skill to have. In this blog post, we'll explore how to get the current date using the Gleam programming language.

## How To
To get the current date in Gleam, we can use the `Date.now()` function. This function returns a `Date` record, which contains the current date and time. Let's see an example of how to use this function in our code:

```Gleam
import time

fn main() {
  let current_date = Date.now()
  time.print(current_date)
}
```

This code imports the `time` module, which contains the `print` function for displaying the current date. Then, we call the `Date.now()` function and assign the resulting `Date` record to the `current_date` variable. Finally, we use the `print` function to display the current date in our terminal.

The output of this code will be something like this:

```bash
2021-11-04T14:25:39.170406+00:00
```

## Deep Dive
You may have noticed that the output of the `Date.now()` function includes the time, not just the date. This is because the date and time are always linked in the Gleam `Date` record. However, if you only want to display the current date, you can use the `Date.toString()` function instead. This function allows you to specify a format for the date output.

For example, if we want to display the date in "Day, Month Date, Year" format, we can use the following code:

```Gleam
import time

fn main() {
  let current_date = Date.now()
  let formatted_date = Date.toString(current_date, "%A, %B %d, %Y")
  time.print(formatted_date)
}
```

The output of this code will be:

```bash
Thursday, November 04, 2021
```

You can explore other date formatting options in the Gleam documentation.

## See Also
To learn more about working with dates in Gleam, check out these resources:

- Official Gleam documentation for the `Date` module: https://gleam.run/documentation/stdlib/date/
- Gleam Playground where you can test out code snippets: https://gleam.run/play
- Example code for manipulating dates in Gleam: https://gist.github.com/kevgo/adeffb9407f896ceaedb3fc03cdd46ad

Now you know how to get the current date in Gleam and even format it to your liking. Happy coding!