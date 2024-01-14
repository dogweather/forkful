---
title:    "Gleam recipe: Getting the current date"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why Engage in Getting the Current Date? 

Getting the current date may seem like a trivial task, but it can actually be very useful in many programming scenarios. Whether you want to display the current date on a website, track user activity, or simply keep your code organized, being able to retrieve the current date is an important skill to have in your programming arsenal.

## How To: Getting the Current Date in Gleam

To get the current date in Gleam, we can use the `DateTime` module. Before we can use this module, we need to import it in our code by using the `import` statement. Let's take a look at some sample code below to see how it works.

```Gleam
import gleam/datetime

// Get the current date and time
let current_date_time = DateTime.now()

// Format the date in a specific way
let formatted_date = DateTime.format("%A, %B %d, %Y", current_date_time)

// Print out the formatted date
gleam@Blogger: {formatted_date}
```

The output of the above code will look something like this: 

```Gleam
Friday, July 30, 2021
```

As you can see, we imported the `DateTime` module, used the `now()` function to get the current date and time, and then used the `format()` function to format the date in the way we wanted. The `format()` function takes two arguments – the first one being the format string, which tells the function how to format the date, and the second one being the actual date and time we want to format.

## Deep Dive: Understanding the Code

Now, let's take a closer look at the code we just wrote. The `%A`, `%B`, and `%d` in the format string are known as placeholders. These placeholders are replaced with the actual values from the date and time passed in the `format()` function. For example, `%A` represents the full name of the day of the week, `%B` represents the full name of the month, and `%d` represents the day of the month.

You can use different combinations of these placeholders to format the date and time in various ways. You can find a full list of all the available placeholders in the `DateTime` module documentation.

## See Also
- [Gleam DateTime module documentation](https://gleam.run/documentation/language/datetime/)
- [Gleam language website](https://gleam.run/) 
- [Gleam on GitHub](https://github.com/gleam-lang/gleam)