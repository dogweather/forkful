---
date: 2024-01-26 00:37:22.346291-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Bash \u811A\u672C\u7684\u9519\u8BEF\u5904\
  \u7406\u53EF\u4EE5\u8FFD\u6EAF\u5230 Unix shell \u7684\u8D77\u6E90\uFF0C\u90A3\u65F6\
  \u5065\u58EE\u53EF\u9760\u7684\u811A\u672C\u5BF9\u4E8E\u7CFB\u7EDF\u7BA1\u7406\u548C\
  \u81EA\u52A8\u5316\u81F3\u5173\u91CD\u8981\uFF08\u73B0\u5728\u4E5F\u662F\uFF09\u3002\
  \u4F20\u7EDF\u4E0A\uFF0C\u5728 Bash \u4E2D\u5904\u7406\u9519\u8BEF\u662F\u901A\u8FC7\
  \u68C0\u67E5\u547D\u4EE4\u7684\u9000\u51FA\u72B6\u6001\uFF0C\u6309\u4E60\u60EF\u8FD4\
  \u56DE 0 \u8868\u793A\u6210\u529F\uFF0C\u975E\u96F6\u503C\u8868\u793A\u5931\u8D25\
  \u3002 Bash \u5F15\u5165\u4E86 `trap`\u2026"
lastmod: '2024-04-05T22:38:47.126471-06:00'
model: gpt-4-1106-preview
summary: "Bash \u5F15\u5165\u4E86 `trap` \u547D\u4EE4\u4F5C\u4E3A\u5185\u5EFA\u529F\
  \u80FD\uFF0C\u5141\u8BB8\u7528\u6237\u6307\u5B9A\u5728\u5404\u79CD\u4FE1\u53F7\u6216\
  \u811A\u672C\u9000\u51FA\u65F6\u8FD0\u884C\u7684\u547D\u4EE4\u3002\u8FD9\u5BF9\u4E8E\
  \u6E05\u7406\u4EFB\u52A1\u6216\u4F5C\u4E3A\u6700\u540E\u624B\u6BB5\u7684\u9519\u8BEF\
  \u5904\u7406\u673A\u7406\u5F88\u6709\u7528\u3002"
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

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
