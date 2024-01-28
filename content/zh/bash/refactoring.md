---
title:                "代码重构"
date:                  2024-01-26T01:16:29.107037-07:00
model:                 gpt-4-0125-preview
simple_title:         "代码重构"
programming_language: "Bash"
category:             "Bash"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/refactoring.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
重构是重新组织现有计算机代码的过程，同时不改变其外部行为。这是一个减少复杂性、提高可维护性以及保持你的代码库健康且易于当前和未来的开发人员理解的重要实践。

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
