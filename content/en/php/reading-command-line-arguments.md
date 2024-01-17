---
title:                "Reading command line arguments"
html_title:           "PHP recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Reading command line arguments is the process of taking input from the user through the command line interface. It allows programmers to make their PHP scripts more versatile and interactive, as they can specify different inputs when running the script each time.

## How to:
To read command line arguments in PHP, we use the ```$argv``` array, which stores all the arguments passed in through the command line. The first argument, ```$argv[0]```, always contains the name of the PHP script being executed. Let's look at an example:

```
<?php 
// script.php
var_dump($argv);
?>
```

Running this script in the command line with the arguments "arg1 arg2" will output the following:
```
$ php script.php arg1 arg2

array(3) {
  [0]=>
  string(10) "script.php"
  [1]=>
  string(4) "arg1"
  [2]=>
  string(4) "arg2"
}
```

We can also use the special variable ```$argc``` to get the number of arguments passed in. This can be useful when we want to perform different actions based on the number of arguments given.

## Deep Dive:
Command line arguments have been used by programmers for a long time to provide more flexibility to their scripts. In PHP, the ```$argv``` array was introduced in PHP 4 to allow easy access to command line arguments. Before this, developers had to use the special function ```getopt()``` or parse the ```$argc``` and ```$argv``` variables manually.

An alternative to using command line arguments in PHP is using environment variables, which can be accessed through the ```$_ENV``` superglobal array. However, using command line arguments is generally preferred as it allows specific and dynamic inputs for each script execution.

## See Also:
- [PHP Documentation on Command Line Interface](https://www.php.net/manual/en/features.commandline.php)
- [PHP Manual on Variable Scope](https://www.php.net/manual/en/language.variables.scope.php)
- [PHP Manual on Superglobals](https://www.php.net/manual/en/language.variables.superglobals.php)