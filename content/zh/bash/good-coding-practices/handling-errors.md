---
date: 2024-01-26 00:37:22.346291-07:00
description: "\u5728 Bash \u811A\u672C\u4E2D\u5904\u7406\u9519\u8BEF\uFF0C\u662F\u5173\
  \u4E8E\u9884\u89C1\u53EF\u80FD\u53D1\u751F\u95EE\u9898\u7684\u5730\u65B9\uFF0C\u5E76\
  \u4E14\u4F18\u96C5\u5730\u5904\u7406\u5B83\u4EEC\u3002\u4E3A\u4EC0\u4E48\uFF1F\u8FD9\
  \u6837\u53EF\u4EE5\u4FDD\u6301\u4F60\u7684\u811A\u672C\u5065\u58EE\uFF0C\u5E76\u4E14\
  \u5F53\u4E8B\u60C5\u4E0D\u6309\u9884\u671F\u5DE5\u4F5C\u65F6\uFF0C\u8282\u7701\u7528\
  \u6237\u7684\u7591\u60D1\u3002"
lastmod: 2024-02-19 22:05:07.019873
model: gpt-4-1106-preview
summary: "\u5728 Bash \u811A\u672C\u4E2D\u5904\u7406\u9519\u8BEF\uFF0C\u662F\u5173\
  \u4E8E\u9884\u89C1\u53EF\u80FD\u53D1\u751F\u95EE\u9898\u7684\u5730\u65B9\uFF0C\u5E76\
  \u4E14\u4F18\u96C5\u5730\u5904\u7406\u5B83\u4EEC\u3002\u4E3A\u4EC0\u4E48\uFF1F\u8FD9\
  \u6837\u53EF\u4EE5\u4FDD\u6301\u4F60\u7684\u811A\u672C\u5065\u58EE\uFF0C\u5E76\u4E14\
  \u5F53\u4E8B\u60C5\u4E0D\u6309\u9884\u671F\u5DE5\u4F5C\u65F6\uFF0C\u8282\u7701\u7528\
  \u6237\u7684\u7591\u60D1\u3002"
title: "\u5904\u7406\u9519\u8BEF"
---

{{< edit_this_page >}}

## 是什么 & 为什么？

在 Bash 脚本中处理错误，是关于预见可能发生问题的地方，并且优雅地处理它们。为什么？这样可以保持你的脚本健壮，并且当事情不按预期工作时，节省用户的疑惑。

## 如何操作：

```Bash
#!/bin/bash

# 将 stderr 重定向到一个文件
grep "something" file.txt 2> errors.log

# 利用退出状态进行错误处理
if ! grep "something" file.txt; then
    echo "哎呀，搜索 'something' 出问题了。"
    exit 1
fi

# 使用 trap 在发生错误时退出前清理
cleanup() {
  echo "清理临时文件..."
  rm temp_*
}

trap cleanup ERR

# 故意错误：文件不存在
cat temp_file.txt
```

当发生错误时的示例输出：

```
清理临时文件...
cat: temp_file.txt: 没有那个文件或目录
```

## 深入探讨

Bash 脚本的错误处理可以追溯到 Unix shell 的起源，那时健壮可靠的脚本对于系统管理和自动化至关重要（现在也是）。传统上，在 Bash 中处理错误是通过检查命令的退出状态，按习惯返回 0 表示成功，非零值表示失败。

Bash 引入了 `trap` 命令作为内建功能，允许用户指定在各种信号或脚本退出时运行的命令。这对于清理任务或作为最后手段的错误处理机理很有用。

还有 `set` 命令，可以改变 Bash 在错误发生时的行为。例如，`set -e` 会让脚本在任何命令以非零状态退出时立即退出，这是一种快速失败并避免错误级联的方式。

除了 Bash 内建的错误处理，替代方法包括明确检查文件的存在、使用命令替代，甚至编写自己的函数来更精细地处理错误。

虽然对于小脚本来说，严格的错误处理有时感觉是多余的，但它是一种能节省调试时间并预防意外行为的做法，无论是对你还是用户都是如此。

## 另请参阅

- Bash 手册关于 Shell 参数的部分：https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameters
- 高级 Bash 脚本编程指南中的错误处理部分：https://www.tldp.org/LDP/abs/html/exit-status.html
- 关于 `trap` 的深入指南：https://mywiki.wooledge.org/SignalTrap

请记住，脚本编写是一种艺术形式，你如何处理失误和绊脚石可以使你的杰作更加坚韧。愉快的脚本编写！
