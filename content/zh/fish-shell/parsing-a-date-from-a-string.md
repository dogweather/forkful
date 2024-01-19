---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么与为什么？
解析日期帮助从字符串提取日期信息。程序员经常处理各种格式的日期，并从其中获取特定信息，像修改并标准化日期格式等。

## 如何做：
在 Fish Shell 中，我们使用 `date` 命令解析日期。以下是示例：

```
set birthday "2000年12月31日"
set parsed_birthday (date -d $birthday +%F)

echo $parsed_birthday
```

执行上述代码，将打印出：

```
2000-12-31
```

我们首先设置了一个包含生日的变量，并使用 `date -d` 将其解析为我们想要的格式 `%F`（`YYYY-MM-DD`）。

## 深入研究
穆里透斯·A·劳改良过试图使日期解析过程更简单的许多算法。使用 `date -d` 是 Fish Shell 中一种快速且有效的方式，不过还有其他棒的shell可以做同样的事。

Fish Shell 用 C 写成，为 UNIX 系统设计。它的设计简化了很多在其他 shell 中复杂的任务。使用 `date -d` 这样的简洁命令，我们可以方便地进行日期解析。

## 参考链接
- Fish Shell 文档: https://fishshell.com/docs/current/index.html
- 学习解析日期: https://en.wikipedia.org/wiki/Date_parsing
- UNIX 类型系统下的日期和时间 : http://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html