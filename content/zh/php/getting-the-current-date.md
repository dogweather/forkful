---
title:    "PHP: 获取当前日期"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

为什么：
在编程过程中，有时我们需要获取当前的日期。这个信息可以帮助我们记录事件发生的时间，或者在网站开发中使用。下面就让我们来了解一下如何使用PHP代码来获取当前日期吧！

如何获取当前日期：
我们可以使用PHP内置的date()函数来获取当前日期。以下是一个基本的示例代码：
```PHP
<?php
// 获取当前日期
$currentDate = date('Y-m-d');

// 输出结果为：2021-07-18
echo $