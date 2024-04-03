---
date: 2024-01-26 01:16:29.107037-07:00
description: "\u91CD\u6784\u662F\u91CD\u65B0\u7EC4\u7EC7\u73B0\u6709\u8BA1\u7B97\u673A\
  \u4EE3\u7801\u7684\u8FC7\u7A0B\uFF0C\u540C\u65F6\u4E0D\u6539\u53D8\u5176\u5916\u90E8\
  \u884C\u4E3A\u3002\u8FD9\u662F\u4E00\u4E2A\u51CF\u5C11\u590D\u6742\u6027\u3001\u63D0\
  \u9AD8\u53EF\u7EF4\u62A4\u6027\u4EE5\u53CA\u4FDD\u6301\u4F60\u7684\u4EE3\u7801\u5E93\
  \u5065\u5EB7\u4E14\u6613\u4E8E\u5F53\u524D\u548C\u672A\u6765\u7684\u5F00\u53D1\u4EBA\
  \u5458\u7406\u89E3\u7684\u91CD\u8981\u5B9E\u8DF5\u3002"
lastmod: '2024-03-13T22:44:47.971205-06:00'
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u91CD\u65B0\u7EC4\u7EC7\u73B0\u6709\u8BA1\u7B97\u673A\
  \u4EE3\u7801\u7684\u8FC7\u7A0B\uFF0C\u540C\u65F6\u4E0D\u6539\u53D8\u5176\u5916\u90E8\
  \u884C\u4E3A\u3002\u8FD9\u662F\u4E00\u4E2A\u51CF\u5C11\u590D\u6742\u6027\u3001\u63D0\
  \u9AD8\u53EF\u7EF4\u62A4\u6027\u4EE5\u53CA\u4FDD\u6301\u4F60\u7684\u4EE3\u7801\u5E93\
  \u5065\u5EB7\u4E14\u6613\u4E8E\u5F53\u524D\u548C\u672A\u6765\u7684\u5F00\u53D1\u4EBA\
  \u5458\u7406\u89E3\u7684\u91CD\u8981\u5B9E\u8DF5\u3002."
title: "\u4EE3\u7801\u91CD\u6784"
weight: 19
---

## 如何进行:
让我们考虑一个需要一些重构的简单Bash脚本。它笨重，代码重复，并且难以跟随:

```Bash
#!/bin/bash
echo "输入一个文件名:"
read filename
if [ -f "$filename" ]; then
    echo "文件存在。"
    count=$(grep -c "foo" "$filename")
    echo "单词foo出现了$count次。"
else
    echo "文件不存在。"
fi
```

为了清晰性和可重用性进行重构可能涉及引入函数和更优雅地处理错误:

```Bash
#!/bin/bash

function file_exists() {
    [[ -f "$1" ]]
}

function count_occurrences() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "输入一个文件名:"
    read -r filename
    echo "输入要搜索的单词:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "单词$word出现了$count次。"
    else
        echo "文件不存在。" >&2
        exit 1
    fi
}

main "$@"
```

重构后的版本使用函数来提高可读性并实现潜在的重用。

## 深入探讨:
重构不是一个起源于Bash或甚至高级编程语言的概念;它和编程本身一样古老。这个术语在1999年Martin Fowler的书《重构：改善既有代码的设计》中得到了正式定义，主要关注面向对象语言。

在Bash脚本的背景下，重构通常意味着将长脚本拆解成函数，用循环或条件语句减少重复，以及避免常见的陷阱，比如未能处理文件名中的空格。对于变得太复杂的脚本，Bash的替代品包括Python或Perl，它们为复杂任务提供更好的数据结构和错误处理。

Bash特定的重构更多关于遵循最佳实践，如对变量进行引用，使用`[[ ]]`而非`[ ]`进行测试，以及偏好使用`printf`而不是`echo`来进行稳健输出。实现细节通常围绕遵守样式指南，并使用像`shellcheck`这样的静态分析工具来捕捉常见错误。

## 另见:
- [Google的Shell样式指南](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck，一个shell脚本的静态分析工具](https://www.shellcheck.net/)
- [命令行的艺术](https://github.com/jlevy/the-art-of-command-line)
