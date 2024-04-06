---
date: 2024-01-20 17:35:18.924849-07:00
description: "How to: (\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\
  \u05EA:) In PHP, you concatenate strings with the dot (`.`) operator. Here's how."
lastmod: '2024-04-05T21:53:40.624513-06:00'
model: gpt-4-1106-preview
summary: "(\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA:)\
  \ In PHP, you concatenate strings with the dot (`.`) operator."
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
weight: 3
---

## How to: (איך לעשות זאת:)
In PHP, you concatenate strings with the dot (`.`) operator. Here's how:

```PHP
// Basic string concatenation
$greeting = "שלום";
$name = "דוד";

$welcomeMessage = $greeting . ", " . $name . "!";
echo $welcomeMessage; // Outputs: שלום, דוד!
```

Combining with other variables and functions:

```PHP
$firstPart = "אני ";
$secondPart = "מתכנת PHP.";

$wholeSentence = $firstPart . $secondPart;
echo $wholeSentence; // Outputs: אני מתכנת PHP.

// Using concatenation with a function
function addExclamation($string) {
    return $string . "!";
}

echo addExclamation("נהדר"); // Outputs: נהדר!
```

## Deep Dive (צלילה לעומק)
String concatenation dates back to the earliest programming days. PHP uses the `.` operator, unlike JavaScript's `+` or Python's `join()` method. Variables within double quotes are parsed, but single quotes are literal; hence, for complexity’s sake, concatenation is clearer. PHP 8 introduced "Stringable" interface allowing objects to be concatenated directly if they implement a `__toString()` method.

## See Also (ראה גם)
- The PHP official documentation on strings: [php.net/manual/en/language.types.string.php](https://www.php.net/manual/en/language.types.string.php)
