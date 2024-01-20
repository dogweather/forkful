---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output is a way we use to check our code's behavior at any given stage. It imparts the power to identify the anomalies and to correct them, ensuring expected program performance.

## How to:

In PHP, there's a handy function known as `var_dump()` to print debug output. Here's a simple example.

```PHP
<?php
  $name = "John Doe";
  var_dump($name);
?>
```

This will yield the output:

```PHP
string(8) "John Doe"
```

`var_dump()` not only returns the variable value but also provides its type and size.

## Deep Dive

1. **Historical context:**

PHP originally didn't have built-in debugging tools. As such, `var_dump()` and `print_r()` became popular solutions due to their simplicity and effectiveness.

2. **Alternatives:**

The `print_r()` function is an alternative, if you want a more human-friendly output. But for more complex information, like indicating data types or array/object depth, `var_dump()` stands out.

Use `print_r()` as:

```PHP
<?php
  $data = array('a', 'b', 'c');
  print_r($data);
?>
```

3. **Implementation details:**

When using `var_dump()` and `print_r()`, keep in mind that while they're useful, they can clutter up output in a production environment. For a cleaner, log-based approach, consider libraries like Monolog.

## See Also

1. [PHP: `var_dump()` Function](https://www.php.net/manual/en/function.var-dump.php)
2. [PHP: `print_r()` Function](https://www.php.net/manual/en/function.print-r.php)
3. [Monolog: PHP Logger Library](https://github.com/Seldaek/monolog)