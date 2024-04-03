---
date: 2024-01-26 01:11:11.088670-07:00
description: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570\u662F\u6307\u5C06\u4EE3\
  \u7801\u5206\u89E3\u6210\u5177\u6709\u5B9A\u4E49\u76EE\u7684\u7684\u53EF\u91CD\u7528\
  \u4EE3\u7801\u5757\u3002\u6211\u4EEC\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4FDD\u6301\
  \u4EE3\u7801\u6574\u6D01\u3001\u907F\u514D\u91CD\u590D\u4EE5\u53CA\u8BA9\u8C03\u8BD5\
  \u53D8\u5F97\u7B80\u5355\u3002"
lastmod: '2024-03-13T22:44:47.870040-06:00'
model: gpt-4-1106-preview
summary: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570\u662F\u6307\u5C06\u4EE3\
  \u7801\u5206\u89E3\u6210\u5177\u6709\u5B9A\u4E49\u76EE\u7684\u7684\u53EF\u91CD\u7528\
  \u4EE3\u7801\u5757\u3002\u6211\u4EEC\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4FDD\u6301\
  \u4EE3\u7801\u6574\u6D01\u3001\u907F\u514D\u91CD\u590D\u4EE5\u53CA\u8BA9\u8C03\u8BD5\
  \u53D8\u5F97\u7B80\u5355\u3002."
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

## 如何操作：
想象一下，我们有重复的代码用于向用户问候。相反，我们将其封装在一个函数`greet_user`中：

```php
function greet_user($name) {
    return "Hello, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

输出：
```
Hello, Alice!
Hello, Bob!
```

现在，你已经有了一个方便的工具，你可以在任何时候使用它，而不用每次想要打招呼时都重写相同的代码行。

## 深入了解
从20世纪50年代FORTRAN编程早期开始，函数一直存在于编程中。它们是结构化编程的基石，关注于模块化和隔离。有替代方案吗？你可以走面向对象的路线，谈谈类和方法，它们是穿上花哨外套的函数。至于PHP，实现细节包括为参数指定默认值、对输入进行类型提示，以及使用数组或PHP 7.1及以后版本可以通过列表返回多个值。

这里有一个带有类型声明和默认值的现代转折：

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4还引入了箭头函数，有助于编写常用于数组操作的简洁单行函数：

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

输出：
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## 另见
- [PHP手册关于函数](https://www.php.net/manual/en/functions.user-defined.php)
- [PHP：正确的方法 - 函数](https://phptherightway.com/#functions)
- [了解PHP 7.4箭头函数](https://stitcher.io/blog/short-closures-in-php)
