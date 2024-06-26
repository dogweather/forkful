---
date: 2024-01-20 17:41:33.102202-07:00
description: "How to: \u5982\u4F55\u5B9E\u73B0\uFF1A \u8FC7\u53BB\uFF0CUnix shell\
  \ \u811A\u672C\u4E3B\u8981\u7528`tr`\u6216`sed`\u505A\u5B57\u7B26\u5904\u7406\u3002\
  Bash\u4E4B\u540E\u52A0\u5165\u4E86\u5185\u5EFA\u7684\u6A21\u5F0F\u66FF\u6362\u529F\
  \u80FD\uFF0C\u63D0\u4F9B\u66F4\u5FEB\u6377\u7684\u65B9\u6CD5\u3002 \u66FF\u6362\u8BED\
  \u6CD5 `string/pattern/replacement` \u91CC\uFF1A - `string` \u662F\u539F\u59CB\u6587\
  \u672C\uFF1B - `pattern` \u662F\u8981\u5339\u914D\u7684\u6A21\u5F0F\uFF1B\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.150683-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u5B9E\u73B0\uFF1A \u8FC7\u53BB\uFF0CUnix shell \u811A\u672C\
  \u4E3B\u8981\u7528`tr`\u6216`sed`\u505A\u5B57\u7B26\u5904\u7406\u3002Bash\u4E4B\u540E\
  \u52A0\u5165\u4E86\u5185\u5EFA\u7684\u6A21\u5F0F\u66FF\u6362\u529F\u80FD\uFF0C\u63D0\
  \u4F9B\u66F4\u5FEB\u6377\u7684\u65B9\u6CD5\u3002"
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
weight: 5
---

## How to: 如何实现：
用变量和模式替换来删除字符：

```Bash
# 删除所有数字
string="Year2023"
echo "${string//[^0-9]/}"

# 输出：2023

# 删除小写字母
string="HelloWorld"
echo "${string//[a-z]/}"

# 输出：HW
```

用`sed`进行模式匹配和删除：

```Bash
echo "Remove vowels from this sentence." | sed 's/[aeiou]//g'

# 输出：Rmv vwls frm ths sntnc.
```

## Deep Dive: 深入探究
过去，Unix shell 脚本主要用`tr`或`sed`做字符处理。Bash之后加入了内建的模式替换功能，提供更快捷的方法。

替换语法 `string/pattern/replacement` 里：
- `string` 是原始文本；
- `pattern` 是要匹配的模式；
- `replacement` 可以是空，这样模式匹配的部分就会被删除。

如果使用`sed`：
- `sed` 很强大，用于文本处理；
- `'s/[pattern]//g'` 命令将会全局删除匹配模式的字符。

## See Also: 相关链接
- Bash 手册: https://www.gnu.org/software/bash/manual/bash.html
- `sed` 入门: https://www.gnu.org/software/sed/manual/sed.html
- 高级 Bash 脚本编写指南: https://www.tldp.org/LDP/abs/html/
