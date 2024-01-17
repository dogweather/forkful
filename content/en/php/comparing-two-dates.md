---
title:                "Comparing two dates"
html_title:           "PHP recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates in PHP is the process of determining whether one date is equal to, before, or after another date. This is useful for comparing events, scheduling tasks, and handling time-based data. Programmers use this feature to manipulate dates and ensure accurate and efficient coding.

## How to:

To compare two dates in PHP, use the `strtotime()` and `date()` functions. The `strtotime()` function converts the date into a Unix timestamp, which represents the number of seconds since January 1, 1970. Then, use the `date()` function to format the timestamp into a readable date format. Below is an example code:

```
$date1 = "2020-09-23";
$date2 = "2020-09-24";

$timestamp1 = strtotime($date1);
$timestamp2 = strtotime($date2);

echo date('d-m-Y', $timestamp1); //Output: 23-09-2020
echo date('d-m-Y', $timestamp2); //Output: 24-09-2020

```

To compare the two dates, simply use comparison operators such as `>`, `<`, or `==` to determine the relationship between the two dates. Below is an example code:

```
if ($timestamp1 > $timestamp2) {
  echo "Date 1 is after Date 2";
} else if ($timestamp1 < $timestamp2) {
  echo "Date 1 is before Date 2";
} else {
  echo "Date 1 is equal to Date 2";
}

//Output: Date 1 is before Date 2
```

## Deep Dive:

The concept of comparing dates has been around for centuries, as humans have always needed a way to organize and track time-based events. In PHP, there are alternative functions such as `DateTime` and `DateTimeImmutable` that provide more flexibility in date manipulation. However, the `strtotime()` and `date()` functions are still commonly used due to their ease of use and wide support.

When using the `strtotime()` function, it is important to note that it works best with dates in the Y-m-d format, similar to the example code above. Changing the format may result in unexpected outputs. Additionally, the `date()` function offers various date formats that can be used to display the date in different styles.

## See Also:

- [PHP Manual - Date and Time Functions](https://www.php.net/manual/en/ref.datetime.php)
- [PHP: Comparison Operators](https://www.php.net/manual/en/language.operators.comparison.php)