---
date: 2024-01-27 20:34:46.869041-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A PHP \u63D0\u4F9B\u4E86\u51E0\u4E2A\u751F\
  \u6210\u968F\u673A\u6570\u7684\u51FD\u6570\uFF0C\u4F46\u6700\u5E38\u7528\u7684\u662F\
  \ `rand()`\u3001`mt_rand()`\uFF0C\u4EE5\u53CA\u7528\u4E8E\u52A0\u5BC6\u76EE\u7684\
  \u7684 `random_int()`\u3002 \u8981\u751F\u6210\u4E00\u4E2A\u7B80\u5355\u7684\u968F\
  \u673A\u6570\uFF0C\u4ECB\u4E8E 0 \u548C getrandmax()\uFF08`rand()` \u53EF\u8FD4\u56DE\
  \u7684\u6700\u5927\u53EF\u80FD\u503C\uFF09\u4E4B\u95F4\uFF0C\u4F60\u53EF\u4EE5\u4F7F\
  \u7528\uFF1A."
lastmod: '2024-04-05T21:53:48.168100-06:00'
model: gpt-4-0125-preview
summary: "\u8981\u751F\u6210\u4E00\u4E2A\u7B80\u5355\u7684\u968F\u673A\u6570\uFF0C\
  \u4ECB\u4E8E 0 \u548C getrandmax()\uFF08`rand()` \u53EF\u8FD4\u56DE\u7684\u6700\u5927\
  \u53EF\u80FD\u503C\uFF09\u4E4B\u95F4\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\uFF1A."
title: "\u751F\u6210\u968F\u673A\u6570"
weight: 12
---

## 如何操作：
PHP 提供了几个生成随机数的函数，但最常用的是 `rand()`、`mt_rand()`，以及用于加密目的的 `random_int()`。

要生成一个简单的随机数，介于 0 和 getrandmax()（`rand()` 可返回的最大可能值）之间，你可以使用：

```PHP
echo rand();
```

对于一个更具体的范围，例如介于 1 和 100 之间：

```PHP
echo rand(1, 100);
```

然而，`mt_rand()` 在速度和随机性方面是更好的选择：

```PHP
echo mt_rand(1, 100);
```

输出结果对于两者都可能是介于 1 和 100 之间的任何数，这取决于随机化，例如 `42`。

对于加密或安全环境，其中不可预测性至关重要，`random_int()` 是首选，因为它生成加密安全的伪随机整数：

```PHP
echo random_int(1, 100);
```

同样，输出是介于 1 和 100 之间的随机数，像 `84`，但具有更强的随机性保证。

## 深入探讨
`rand()` 函数自 PHP 早期版本以来一直存在，作为生成随机数的初步方法。然而，由于其算法相对可预测，对于需要高度随机性的应用来说，它并非最佳选择。

`mt_rand()`，在 PHP 4 中引入，基于 Mersenne Twister 算法 - 在速度和能够生成的随机性方面远优于 `rand()`。它很快成为大多数非加密需求的首选选项。

对于安全敏感的应用程序，PHP 7 引入了 `random_int()`，使用来自系统随机数生成器的随机字节生成加密安全的伪随机整数。它比 `rand()` 或 `mt_rand()` 显著更加安全，使其成为生成令牌、密钥或其他可能因可预测性而导致安全漏洞的元素的最佳选择。

尽管有这些改进，根据应用程序的上下文选择正确的函数至关重要。对于一般用途，`mt_rand()` 就足够了，但对于可能成为攻击目标或被利用的任何东西，`random_int()` 是去路，提供了随机性和安全性。
