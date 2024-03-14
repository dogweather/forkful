---
date: 2024-01-26 03:50:48.025257-07:00
description: "\u8C03\u8BD5\u5668\u662F\u4E00\u79CD\u5E2E\u52A9\u7A0B\u5E8F\u5458\u7406\
  \u89E3\u4ED6\u4EEC\u7684\u4EE3\u7801\u5728\u8FD0\u884C\u65F6\u5B9E\u9645\u4E0A\u5728\
  \u505A\u4EC0\u4E48\u7684\u5DE5\u5177\u3002\u5B83\u662F\u8BA9\u6211\u4EEC\u80FD\u591F\
  \u653E\u5927\u67E5\u770Bbugs\uFF08\u90A3\u4E9B\u8BA9\u6211\u4EEC\u7684\u7A0B\u5E8F\
  \u5D29\u6E83\u6216\u8005\u5410\u51FA\u9519\u8BEF\u7B54\u6848\u7684\u8BA8\u538C\u95EE\
  \u9898\uFF09\u5E76\u538B\u6241\u5B83\u4EEC\u7684\u653E\u5927\u955C\u3002\u6211\u4EEC\
  \u4F7F\u7528\u8C03\u8BD5\u5668\u662F\u56E0\u4E3A\u5B83\u4EEC\u53EF\u4EE5\u8282\u7701\
  \u6211\u4EEC\u6570\u5C0F\u65F6\u7684\u6253\u5370\u8BED\u53E5\u548C\u731C\u6D4B\u6E38\
  \u620F\u3002"
lastmod: '2024-03-13T22:44:47.868989-06:00'
model: gpt-4-0125-preview
summary: "\u8C03\u8BD5\u5668\u662F\u4E00\u79CD\u5E2E\u52A9\u7A0B\u5E8F\u5458\u7406\
  \u89E3\u4ED6\u4EEC\u7684\u4EE3\u7801\u5728\u8FD0\u884C\u65F6\u5B9E\u9645\u4E0A\u5728\
  \u505A\u4EC0\u4E48\u7684\u5DE5\u5177\u3002\u5B83\u662F\u8BA9\u6211\u4EEC\u80FD\u591F\
  \u653E\u5927\u67E5\u770Bbugs\uFF08\u90A3\u4E9B\u8BA9\u6211\u4EEC\u7684\u7A0B\u5E8F\
  \u5D29\u6E83\u6216\u8005\u5410\u51FA\u9519\u8BEF\u7B54\u6848\u7684\u8BA8\u538C\u95EE\
  \u9898\uFF09\u5E76\u538B\u6241\u5B83\u4EEC\u7684\u653E\u5927\u955C\u3002\u6211\u4EEC\
  \u4F7F\u7528\u8C03\u8BD5\u5668\u662F\u56E0\u4E3A\u5B83\u4EEC\u53EF\u4EE5\u8282\u7701\
  \u6211\u4EEC\u6570\u5C0F\u65F6\u7684\u6253\u5370\u8BED\u53E5\u548C\u731C\u6D4B\u6E38\
  \u620F\u3002"
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
---

{{< edit_this_page >}}

## 什么 & 为什么？
调试器是一种帮助程序员理解他们的代码在运行时实际上在做什么的工具。它是让我们能够放大查看bugs（那些让我们的程序崩溃或者吐出错误答案的讨厌问题）并压扁它们的放大镜。我们使用调试器是因为它们可以节省我们数小时的打印语句和猜测游戏。

## 如何操作：
PHP自带一个交互式调试器，名为Xdebug。以下是如何使用它的指南。

首先，确保你已经安装并在你的`php.ini`文件中配置了Xdebug：

```
zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-xxxxxxxx/xdebug.so
xdebug.mode=debug
xdebug.start_with_request=yes
```

接下来，编写一个带有bug的简单PHP脚本：

```PHP
<?php
function add($a, $b) {
    return $a - $b; // 糟糕！这应该是加号，而不是减号
}

$result = add(1, 2);
echo "结果是：$result"; // 输出应该是3，而不是-1
```

使用像PhpStorm这样的IDE，在行号旁边点击以设置断点。运行调试器并观察当你逐步执行时变量如何变化。当你逐过`add`函数时，你会注意到`$result`变成了-1，这是出乎意料的。

## 深入探索：
从历史上看，PHP主要用于小型脚本，调试的方法是在代码中添加`var_dump()`和`print_r()`语句。随着时间的推移，PHP在网站开发中成为了关键角色，像Xdebug和Zend Debugger这样更复杂的工具开始被使用。

Xdebug的替代品包括pcov和phpdbg。这些提供了各种功能，但可能不如Xdebug功能全面。phpdbg是一个轻量级的，特定于PHP的调试器，自5.6版以来已经随PHP一起分发，而pcov是一个代码覆盖率驱动。

当实现调试器时，记住你应该永远不要在你的生产服务器上启用调试器，因为它可能会暴露安全漏洞并减慢性能。

## 另请参阅：
- [Xdebug文档](https://xdebug.org/docs/)
- [PhpStorm调试指南](https://www.jetbrains.com/help/phpstorm/debugging.html)
- [PHP.net上的phpdbg](https://www.php.net/manual/en/book.phpdbg.php)
- [pcov在GitHub上](https://github.com/krakjoe/pcov)
