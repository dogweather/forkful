---
title:                "Printing debug output"
date:                  2024-01-20T17:53:03.682497-07:00
model:                 gpt-4-1106-preview
simple_title:         "Printing debug output"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Printing debug output is about slapping your code's internal data onto the screen to figure out what's going on. Programmers do it for a sanity check, to spot where the bugs are hiding or just to see if the code is doing what it's supposed to.

## How to:
```PHP
<?php
// Basic output
$variable = 'Debugging rocks!';
echo $variable;

// Using print_r for arrays
$myArray = ['apple', 'orange', 'banana'];
echo '<pre>'; // Makes it readable
print_r($myArray);
echo '</pre>';

// var_dump for details
$anotherArray = ['key' => 'value', 'anotherKey' => 123];
var_dump($anotherArray);

// To the error log
error_log('This goes to the logs for stealthier debugs.');
?>
```
Sample Output:
```
Debugging rocks!
Array
(
    [0] => apple
    [1] => orange
    [2] => banana
)
array(2) {
  ["key"]=>
  string(5) "value"
  ["anotherKey"]=>
  int(123)
}
```

## Deep Dive:
Debug output hasn't changed much: it's been there since the early days when ancient programmers debugged with printf(). PHP embraced this with `echo`, `print`, `print_r()`, and `var_dump()`. It might not be fancy, but it works. Modern PHP devs also have Xdebug, which can step through code and show a fancier output. For logs, you've got `error_log()`, which sneaks messages into server logs without exposing them to users. Each tool has its place: `echo` and `print` are quick and dirty; `print_r()` is for human-friendly array insights; `var_dump()` gives you the nitty-gritty on types and lengths; `error_log()` keeps things under wraps when you're in detective mode on a live site.

## See Also:
- The PHP manual on `echo`: https://www.php.net/manual/en/function.echo.php
- More on `print_r()`: https://www.php.net/manual/en/function.print-r.php
- The gritty details of `var_dump()`: https://www.php.net/manual/en/function.var-dump.php
- Dive into logging with `error_log()`: https://www.php.net/manual/en/function.error-log.php
- Xdebug, the debugger's best friend: https://xdebug.org/docs/display