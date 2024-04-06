---
date: 2024-01-20 17:53:30.897257-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C:) \u65E9\u5728PHP\u8BDE\u751F\u4E4B\
  \u521D\uFF0C`echo` \u548C `print` \u5DF2\u7ECF\u88AB\u7528\u4E8E\u8F93\u51FA\u4FE1\
  \u606F\u3002\u8FD9\u79CD\u76F4\u63A5\u7684\u65B9\u5F0F\u975E\u5E38\u9002\u5408\u5FEB\
  \u901F\u8C03\u8BD5\u3002\u76F4\u81F3\u4ECA\u65E5\uFF0C\u5C3D\u7BA1\u5B58\u5728\u66F4\
  \u590D\u6742\u7684\u8C03\u8BD5\u5DE5\u5177\uFF08\u5982Xdebug\uFF09\uFF0C\u8BB8\u591A\
  \u5F00\u53D1\u8005\u5728\u8C03\u8BD5\u65F6\u4ECD\u504F\u597D\u7B80\u5355\u6253\u5370\
  \u8F93\u51FA\uFF0C\u56E0\u4E3A\u5B83\u6613\u4E8E\u4F7F\u7528\u4E14\u65E0\u9700\u989D\
  \u5916\u914D\u7F6E\u3002 \u4EA6\u53EF\u4F7F\u7528 `error_log()`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.075268-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C:) \u65E9\u5728PHP\u8BDE\u751F\u4E4B\u521D\uFF0C\
  `echo` \u548C `print` \u5DF2\u7ECF\u88AB\u7528\u4E8E\u8F93\u51FA\u4FE1\u606F\u3002\
  \u8FD9\u79CD\u76F4\u63A5\u7684\u65B9\u5F0F\u975E\u5E38\u9002\u5408\u5FEB\u901F\u8C03\
  \u8BD5\u3002\u76F4\u81F3\u4ECA\u65E5\uFF0C\u5C3D\u7BA1\u5B58\u5728\u66F4\u590D\u6742\
  \u7684\u8C03\u8BD5\u5DE5\u5177\uFF08\u5982Xdebug\uFF09\uFF0C\u8BB8\u591A\u5F00\u53D1\
  \u8005\u5728\u8C03\u8BD5\u65F6\u4ECD\u504F\u597D\u7B80\u5355\u6253\u5370\u8F93\u51FA\
  \uFF0C\u56E0\u4E3A\u5B83\u6613\u4E8E\u4F7F\u7528\u4E14\u65E0\u9700\u989D\u5916\u914D\
  \u7F6E\u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

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
