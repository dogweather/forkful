---
title:    "PHP: 连接字符串"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 为什么要连接字符串

当我们在进行编程的时候，有时候需要把几个字符串连接起来，这就是所谓的字符串连接。这个过程很常见，因为它能帮助我们构建更复杂的数据结构，例如 URL 或者动态信息的输出。下面是一个简单的例子：
 
## 如何连接字符串

PHP 中连接字符串的方法是使用 dot （.），它可以把两个或者多个字符串合并在一起。下面是一个例子：

```PHP
$name = "张三";
$age = 25;
echo "你好，我叫" . $name . "，今年" . $age . "岁。";
```

输出结果：

```PHP
你好，我叫张三，今年25岁。
```

## 深入了解连接字符串

连接字符串是一个简单但是强大的技巧，它可以允许我们在输出信息的时候更加灵活。除了使用 dot 连接字符串外，我们也可以使用另外两个方法：`sprintf()` 和 `implode()`。`sprintf()` 允许我们使用占位符来方便地替换变量，而 `implode()` 则可以把一个数组中的字符串连接起来。下面是对比：

```PHP
$name = "张三";
$age = 25;
echo sprintf("你好，我叫 %s，今年 %d 岁。", $name, $age);
```

输出结果：

```PHP
你好，我叫张三，今年25岁。
```

```PHP
$cities = array("北京", "上海", "广州");
echo "我想去" . implode("、", $cities) . "旅行。";
```

输出结果：

```PHP
我想去北京、上海、广州旅行。
```

## 参考链接

- [PHP字符串连接方法](https://www.php.net/manual/en/language.operators.string.php)
- [PHP中sprintf()和implode()的使用](https://www.geeksforgeeks.org/sprintf-function-in-php/)
- [PHP中字符串相关函数](https://www.tutorialspoint.com/php/php_strings.htm)

## 参见

- [PHP基础知识教程](https://phptherightway.com/)