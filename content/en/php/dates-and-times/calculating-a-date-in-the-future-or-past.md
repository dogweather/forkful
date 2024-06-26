---
date: 2024-01-20 17:31:36.718183-07:00
description: 'How to: PHP makes date math simple with `DateTime` and `DateInterval`.
  Check this out.'
lastmod: '2024-03-13T22:45:00.179355-06:00'
model: gpt-4-1106-preview
summary: PHP makes date math simple with `DateTime` and `DateInterval`.
title: Calculating a date in the future or past
weight: 26
---

## How to:
PHP makes date math simple with `DateTime` and `DateInterval`. Check this out:

```PHP
<?php
// Today's date
$today = new DateTime();
echo $today->format('Y-m-d H:i:s') . "\n";

// Add 10 days
$today->add(new DateInterval('P10D'));
echo $today->format('Y-m-d H:i:s') . "\n";

// Subtract 2 months
$today->sub(new DateInterval('P2M'));
echo $today->format('Y-m-d H:i:s') . "\n";
?>
```
Output might be:
```
2023-04-01 12:34:56
2023-04-11 12:34:56
2023-02-11 12:34:56
```

## Deep Dive
Back in the day, PHP date calculations were more error-prone. `strtotime`, while still useful, can trip you up with edge cases. `DateTime` and `DateInterval` brought precision and object-oriented clarity.

Alternatives? Sure. Libraries like Carbon wrap PHP's date functionality for more readability and features, but for many cases, PHP’s built-in classes will do just fine.

Under the hood, `DateTime::add()` and `DateTime::sub()` alter the object, so no need to reassign. They handle time units consistently, accounting for things like leap years and daylight saving time changes, which can be a real headache otherwise.

## See Also
- PHP Manual on DateTime: https://www.php.net/manual/en/class.datetime.php
- DateInterval documentation: https://www.php.net/manual/en/class.dateinterval.php
- Carbon: A simple API extension for DateTime - https://carbon.nesbot.com
