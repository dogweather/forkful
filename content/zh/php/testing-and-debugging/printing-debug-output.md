---
date: 2024-01-20 17:53:30.897257-07:00
description: "\u5728\u4EE3\u7801\u4E2D\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u6307\
  \u8F93\u51FA\u53D8\u91CF\u3001\u8868\u8FBE\u5F0F\u6216\u7A0B\u5E8F\u72B6\u6001\u4FE1\
  \u606F\uFF0C\u5E2E\u52A9\u5F00\u53D1\u8005\u7406\u89E3\u548C\u89E3\u51B3\u95EE\u9898\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u56E0\u4E3A\u8FD9\u662F\u4E00\u79CD\
  \u7B80\u5355\u76F4\u63A5\u7684\u65B9\u5F0F\uFF0C\u80FD\u5373\u65F6\u770B\u5230\u7A0B\
  \u5E8F\u7684\u6267\u884C\u60C5\u51B5\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.866422-06:00'
model: gpt-4-1106-preview
summary: "\u5728\u4EE3\u7801\u4E2D\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u6307\
  \u8F93\u51FA\u53D8\u91CF\u3001\u8868\u8FBE\u5F0F\u6216\u7A0B\u5E8F\u72B6\u6001\u4FE1\
  \u606F\uFF0C\u5E2E\u52A9\u5F00\u53D1\u8005\u7406\u89E3\u548C\u89E3\u51B3\u95EE\u9898\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u56E0\u4E3A\u8FD9\u662F\u4E00\u79CD\
  \u7B80\u5355\u76F4\u63A5\u7684\u65B9\u5F0F\uFF0C\u80FD\u5373\u65F6\u770B\u5230\u7A0B\
  \u5E8F\u7684\u6267\u884C\u60C5\u51B5\u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

## What & Why? (什么 & 为什么?)
在代码中打印调试输出是指输出变量、表达式或程序状态信息，帮助开发者理解和解决问题。程序员这样做是因为这是一种简单直接的方式，能即时看到程序的执行情况。

## How to: (如何操作:)
```php
<?php
// 假定我们有一个数组需要检查内容
$array = ['apple', 'banana', 'cherry'];

// 使用 print_r 打印数组结构
print_r($array);

// 如果想要清晰一点的输出格式，使用 <pre>
echo '<pre>' . print_r($array, true) . '</pre>';

// 使用 var_dump 打印详细信息，包括类型和长度
var_dump($array);

// 在浏览器中查看简单的字符串信息
echo 'Current PHP version: ' . phpversion();
?>
```
输出可能是这样:
```
Array ( [0] => apple [1] => banana [2] => cherry )
<pre>Array
(
    [0] => apple
    [1] => banana
    [2] => cherry
)
</pre>
array(3) {
  [0]=>
  string(5) "apple"
  [1]=>
  string(6) "banana"
  [2]=>
  string(6) "cherry"
}
Current PHP version: 8.1.3
```

## Deep Dive (深入探索)
早在PHP诞生之初，`echo` 和 `print` 已经被用于输出信息。这种直接的方式非常适合快速调试。直至今日，尽管存在更复杂的调试工具（如Xdebug），许多开发者在调试时仍偏好简单打印输出，因为它易于使用且无需额外配置。

亦可使用 `error_log()` 函数将调试信息输出到日志文件中，对于不便于直接输出到屏幕的情境尤其有用，比如API开发时。

实现细节方面，`print_r()`、`var_dump()` 和 `var_export()` 是PHP提供的内建函数，用于不同场景的输出。`print_r()` 适合于打印易读的信息，`var_dump()` 提供更详细的类型和长度信息, 而 `var_export()` 则可以返回有效的PHP代码。

## See Also (参考链接)
- PHP官方文档: [PHP: echo - Manual](https://www.php.net/manual/en/function.echo.php)
- PHP官方文档: [PHP: print_r - Manual](https://www.php.net/manual/en/function.print-r.php)
- PHP官方文档: [PHP: var_dump - Manual](https://www.php.net/manual/en/function.var-dump.php)
- Xdebug —— 一种PHP调试器：[Xdebug: Documentation](https://xdebug.org/docs)
