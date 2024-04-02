---
date: 2024-01-26 01:18:04.671578-07:00
description: "\u91CD\u6784\u662F\u5728\u4E0D\u6539\u53D8\u4EE3\u7801\u5916\u90E8\u884C\
  \u4E3A\u7684\u524D\u63D0\u4E0B\u91CD\u65B0\u7EC4\u7EC7\u73B0\u6709\u4EE3\u7801\uFF0C\
  \u4EE5\u6539\u5584\u975E\u529F\u80FD\u5C5E\u6027\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\
  \u5458\u8FDB\u884C\u91CD\u6784\u662F\u4E3A\u4E86\u8BA9\u4EE3\u7801\u66F4\u6613\u8BFB\
  \u3001\u964D\u4F4E\u590D\u6742\u5EA6\u3001\u63D0\u9AD8\u53EF\u7EF4\u62A4\u6027\uFF0C\
  \u5E76\u4F7F\u5176\u66F4\u5BB9\u6613\u5728\u672A\u6765\u8FDB\u884C\u6269\u5C55\u6216\
  \u4FEE\u6539\u3002"
lastmod: '2024-03-13T22:44:48.275066-06:00'
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u5728\u4E0D\u6539\u53D8\u4EE3\u7801\u5916\u90E8\u884C\
  \u4E3A\u7684\u524D\u63D0\u4E0B\u91CD\u65B0\u7EC4\u7EC7\u73B0\u6709\u4EE3\u7801\uFF0C\
  \u4EE5\u6539\u5584\u975E\u529F\u80FD\u5C5E\u6027\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\
  \u5458\u8FDB\u884C\u91CD\u6784\u662F\u4E3A\u4E86\u8BA9\u4EE3\u7801\u66F4\u6613\u8BFB\
  \u3001\u964D\u4F4E\u590D\u6742\u5EA6\u3001\u63D0\u9AD8\u53EF\u7EF4\u62A4\u6027\uFF0C\
  \u5E76\u4F7F\u5176\u66F4\u5BB9\u6613\u5728\u672A\u6765\u8FDB\u884C\u6269\u5C55\u6216\
  \u4FEE\u6539\u3002"
title: "\u4EE3\u7801\u91CD\u6784"
weight: 19
---

## 何为重构及其原因？
重构是在不改变代码外部行为的前提下重新组织现有代码，以改善非功能属性的过程。程序员进行重构是为了让代码更易读、降低复杂度、提高可维护性，并使其更容易在未来进行扩展或修改。

## 如何进行：
想象一下，你有一个脚本随着时间的推移增长了不少。它起初很简单，但现在它成了一个逻辑混乱的庞然大物。这是一个关于重构函数以使其更易读和高效的小例子：

重构前：
```fish
function old_and_clunky
    set color (cat ~/.config/fish/color_theme)
    if test "$color" = 'blue'
        echo 'Blue theme set!'
    else if test "$color" = 'red'
        echo 'Red theme set!'
    else
        echo 'Default theme set!'
    end
end
```

重构后：
```fish
function set_theme_color
    set theme_color (cat ~/.config/fish/color_theme)
    switch $theme_color
        case blue
            echo 'Blue theme set!'
        case red
            echo 'Red theme set!'
        default
            echo 'Default theme set!'
    end
end
```
重构改进了函数的名称，以更好地描述其目的，并用更清晰的`switch`语句替换了if-else链。

示例输出：
```
Blue theme set!
```

## 深入探讨
在马丁·福勒的开创性书籍《重构：改善既有代码的设计》中首次详细描述了重构。该书提出了一种改进代码而不编写新功能的结构化方法。从那时起，介绍了许多重构技术，且这个概念已成为现代软件开发的一个基本部分。

在Fish Shell环境中，重构可能与其他编程语境中的看起来稍有不同，因为其专门的语法和命令行特性。在Fish中重构脚本的替代方法可能涉及移植到另一种shell语言或使用外部工具进行更高级的脚本管理。然而，保留原生的Fish语法通常意味着更好地与shell的特性集成和更流畅的体验。

在Fish Shell中进行重构时，你主要是处理函数和命令，而不是其他语言中常见的宽范围类或模块。这种粒度可以使重构任务变得更直接和立即，但它也强调了编写清晰、简洁和可维护代码的重要性。

## 参见
- 马丁·福勒的重构网站：[https://refactoring.com/](https://refactoring.com/)
- 官方Fish Shell文档：[https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
