---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么与为什么?
PHP的字符串插值允许在字符串中直接插入变量的值。程序员这样做是为了简化代码并增加其可读性。

## 如何操作:
以下是字符串插值的PHP代码示例:
```PHP
$name = "John";
echo "Hello, $name!";
```
输出如下： 
```PHP
Hello, John!
```
在双引号内部使用变量名称 ($name)，PHP可以识别出这是变量，并在输出字符串时替换为变量的值。

## 深入学习:
1. 历史背景: PHP的字符串插值受到了Perl的影响，在早期版本中被引入，因为它使得形成动态字符串更加简单和直观。
2. 替代方案: 对于不支持字符串插值的情况，可以使用字符串连接来达到相同的效果。例如：
  ```PHP
  $name = "John";
  echo "Hello, " . $name . "!";
  ```
3. 实现细节: 使用双引号时，PHP将解析其中的变量并用它们的值替换。但是使用单引号时，就不会解析变量，因此，如果你使用：
  ```PHP
   $name = "John";
   echo 'Hello, $name!';
  ```
  输出将会是： 'Hello, $name!' 而不是 'Hello, John!'.

## 另请参阅: 
1. [PHP官方文档关于字符串插值的页面](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing.complex)
2. [StackOverflow上的相关讨论](https://stackoverflow.com/questions/9257505/variable-interpolation-php) 
3. [W3Schools的PHP教程](https://www.w3schools.com/php/php_strings.asp)