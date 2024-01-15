---
title:                "打印调试输出"
html_title:           "PHP: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/printing-debug-output.md"
---

{{< edit_this_page >}}

# 为什么要打印调试输出（Why）

调试是程序员的日常工作之一，它帮助我们发现并解决代码中的错误。打印调试输出是一种常用的调试方法，它可以让我们在程序运行中查看变量的值和代码的执行情况，帮助我们更快地找到错误的原因。

# 如何打印调试输出（How To）

打印调试输出需要使用PHP语言中的一个函数，那就是 `var_dump()`。这个函数的作用是打印出一个或多个变量的类型和值，让我们可以清晰地看到变量的内容。

以下是一个简单的例子：

```PHP
$var = "Hello World";

var_dump($var);
```

运行上面的代码，我们会在页面上看到如下输出：

```
string(11) "Hello World"
```

这表示变量 `$var` 是一个字符串类型，且值为 `"Hello World"`。除了打印变量的值，`var_dump()`还会在每个输出的前面加上类型信息，方便我们更好地理解变量的内容。

如果我们想要打印多个变量，可以在函数中依次添加每个变量，如下所示：

```PHP
$a = 10;
$b = "PHP";
$c = true;

var_dump($a, $b, $c);
```

输出结果如下：

```
int(10)
string(3) "PHP"
bool(true)
```

如果想要打印的内容更加清晰，我们可以在每个输出之间添加分隔符。例如，我们可以使用 `echo` 函数来打印一个分隔符，让每次输出都在一行上：

```PHP
$a = 10;
$b = "PHP";
$c = true;

echo "<hr>";

var_dump($a, $b, $c);
```

输出结果如下：

```
<hr>
int(10)
string(3) "PHP"
bool(true)
```

除了使用 `var_dump()` 来打印调试输出，我们也可以使用 `print_r()` 函数，它的作用类似于 `var_dump()`，但是输出的格式更友好。例如：

```PHP
$array = ["apple", "banana", "orange"];

print_r($array);
```

输出结果如下：

```
Array
(
    [0] => apple
    [1] => banana
    [2] => orange
)
```

# 深入了解打印调试输出（Deep Dive）

除了上面介绍的两个函数，PHP还提供了其他的打印调试输出的方法，例如使用 `die()` 函数可以在调试过程中终止程序的执行并打印出指定的消息，让我们更方便地调试程序。

另外，我们也可以使用 `error_log()` 函数将调试输出写入到服务器的日志文件中，这对于线上代码的调试非常有用。

总的来说，打印调试输出是一项非常重要的调试技巧，它可以帮助我们更快地解决代码中的错误，提高开发效率。但是，在线上环境中我们应该避免使用这些调试方法，可以使用 `if` 语句来限制调试输出的使用，以避免暴露敏感信息。

# 参考资料（See Also）

- [PHP: 调试技巧](https://www.php.net/manual/zh/debugger.php)
- [使用 var_dump() 打印调试信息](https://www.php.net/manual/zh/function.var-dump.php)
- [使用 print_r() 打印调试信息](https://www.php.net/manual/zh/function.print-r.php)
- [使用 die() 函数进行调试](https://www.php.net/manual/zh/function.die.php)
- [使用 error_log() 函数将调试信息写入日志文件](https://www.php.net/manual/zh/function.error-log.php)