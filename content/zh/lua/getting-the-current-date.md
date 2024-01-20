---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么以及为什么?

在编程中获取当前日期指的是通过代码命令获取系统的当前日期。这对于时间敏感的操作，如日志记录，跟踪事件，时间戳等都是非常必要的。

## 如何：

获取Lua中的当前日期并非难事。请查看以下示例和输出：

```Lua
os.date("%x")
```

这将输出当前日期，格式为：mm/dd/yyyy。

也可以这样：

```Lua
os.date("%A, %B %d, %Y")
```

这将输出当前日期，格式为：星期，月 日，年。例如：

```Lua
"星期三，九月 15，2021"
```

## 深入研究：

Lua语言于1993年由巴西的Pontifical Catholic University的Roberto Ierusalimschy，Luiz Henrique de Figueiredo，和Waldemar Celes开发。它是一个小型的脚本语言，主要用于嵌入到应用程序中，为应用程序提供灵活的扩展和定制功能。

获取当前日期的其他方法：
Lua语言还提供了os.time()函数，可以获取从1970年1月1日（称为UNIX纪元）到目前的秒数。

获取日期的实现细节：
在Lua中，os.date()函数实际上是C库的strftime()函数的封装。它按照格式字符串的指示格式化日期和时间。

## 另请参阅：

- [Lua 5.3 参考手册 - os库](http://www.runoob.com/manual/lua53doc/manual.html#pdf-os.date)
- [Lua-users 指南 - os库](http://lua-users.org/wiki/OsLibraryTutorial)
- [Lua-users 资源 - 时间和日期](http://lua-users.org/wiki/TimeAndDateExample)

以上就是如何在Lua中获取当前日期的全部内容。