---
date: 2024-01-26 03:50:48.025257-07:00
description: "PHP\u81EA\u5E26\u4E00\u4E2A\u4EA4\u4E92\u5F0F\u8C03\u8BD5\u5668\uFF0C\
  \u540D\u4E3AXdebug\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\u7684\u6307\
  \u5357\u3002 \u9996\u5148\uFF0C\u786E\u4FDD\u4F60\u5DF2\u7ECF\u5B89\u88C5\u5E76\u5728\
  \u4F60\u7684`php.ini`\u6587\u4EF6\u4E2D\u914D\u7F6E\u4E86Xdebug\uFF1A ``` zend_extension=/usr/local/lib/php/extensions/no-debug-non-zts-\u2026"
lastmod: '2024-03-13T22:44:47.868989-06:00'
model: gpt-4-0125-preview
summary: "PHP\u81EA\u5E26\u4E00\u4E2A\u4EA4\u4E92\u5F0F\u8C03\u8BD5\u5668\uFF0C\u540D\
  \u4E3AXdebug\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\u7684\u6307\u5357\
  \u3002\n\n\u9996\u5148\uFF0C\u786E\u4FDD\u4F60\u5DF2\u7ECF\u5B89\u88C5\u5E76\u5728\
  \u4F60\u7684`php."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

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
