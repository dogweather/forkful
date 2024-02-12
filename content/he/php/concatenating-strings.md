---
title:                "שרשור מחרוזות"
aliases:
- he/php/concatenating-strings.md
date:                  2024-01-20T17:35:18.924849-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
Concatenating strings means gluing separate pieces of text together. Programmers do it to build dynamic content, like full names from first and last or custom messages.

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
