---
date: 2024-01-27 16:20:54.722501-07:00
description: "\u60F3\u8C61\u4E00\u4E0B\uFF0C\u4F60\u521A\u53D1\u73B0\u9700\u8981\u6279\
  \u91CF\u66F4\u65B0\u670D\u52A1\u5668\u4E0A\u7684\u51E0\u4E2A\u914D\u7F6E\u6587\u4EF6\
  \u3002\u4F60\u53EF\u4EE5\u6253\u5F00\u6BCF\u4E2A\u6587\u4EF6\uFF0C\u624B\u52A8\u8FDB\
  \u884C\u66F4\u6539\uFF0C\u7136\u540E\u4FDD\u5B58\u3002\u6216\u8005\uFF0C\u4F60\u53EF\
  \u4EE5\u76F4\u63A5\u4ECE\u547D\u4EE4\u884C\u754C\u9762\uFF08CLI\uFF09\u8FDB\u884C\
  \u5C31\u5730\u7F16\u8F91\uFF0C\u8FD9\u662F\u4E00\u9879\u8282\u7701\u65F6\u95F4\u3001\
  \u51CF\u5C11\u9519\u8BEF\u548C\u81EA\u52A8\u5316\u91CD\u590D\u6027\u4EFB\u52A1\u7684\
  \u6280\u80FD\u3002\u8FD9\u79CD\u6280\u672F\u7279\u522B\u9002\u7528\u4E8E\u7CFB\u7EDF\
  \u66F4\u65B0\u3001\u66F4\u6B63\u6216\u6279\u91CF\u4FEE\u6539\uFF0C\u624B\u52A8\u7F16\
  \u8F91\u53EF\u80FD\u4E0D\u5207\u5B9E\u9645\u6216\u5BB9\u6613\u51FA\u9519\u3002"
lastmod: '2024-03-13T22:44:47.956987-06:00'
model: gpt-4-0125-preview
summary: "\u60F3\u8C61\u4E00\u4E0B\uFF0C\u4F60\u521A\u53D1\u73B0\u9700\u8981\u6279\
  \u91CF\u66F4\u65B0\u670D\u52A1\u5668\u4E0A\u7684\u51E0\u4E2A\u914D\u7F6E\u6587\u4EF6\
  \u3002\u4F60\u53EF\u4EE5\u6253\u5F00\u6BCF\u4E2A\u6587\u4EF6\uFF0C\u624B\u52A8\u8FDB\
  \u884C\u66F4\u6539\uFF0C\u7136\u540E\u4FDD\u5B58\u3002\u6216\u8005\uFF0C\u4F60\u53EF\
  \u4EE5\u76F4\u63A5\u4ECE\u547D\u4EE4\u884C\u754C\u9762\uFF08CLI\uFF09\u8FDB\u884C\
  \u5C31\u5730\u7F16\u8F91\uFF0C\u8FD9\u662F\u4E00\u9879\u8282\u7701\u65F6\u95F4\u3001\
  \u51CF\u5C11\u9519\u8BEF\u548C\u81EA\u52A8\u5316\u91CD\u590D\u6027\u4EFB\u52A1\u7684\
  \u6280\u80FD\u3002\u8FD9\u79CD\u6280\u672F\u7279\u522B\u9002\u7528\u4E8E\u7CFB\u7EDF\
  \u66F4\u65B0\u3001\u66F4\u6B63\u6216\u6279\u91CF\u4FEE\u6539\uFF0C\u624B\u52A8\u7F16\
  \u8F91\u53EF\u80FD\u4E0D\u5207\u5B9E\u9645\u6216\u5BB9\u6613\u51FA\u9519\u3002"
title: "\u4F7F\u7528\u547D\u4EE4\u884C\u4E00\u884C\u547D\u4EE4\u5C31\u5730\u7F16\u8F91\
  \u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？

想象一下，你刚发现需要批量更新服务器上的几个配置文件。你可以打开每个文件，手动进行更改，然后保存。或者，你可以直接从命令行界面（CLI）进行就地编辑，这是一项节省时间、减少错误和自动化重复性任务的技能。这种技术特别适用于系统更新、更正或批量修改，手动编辑可能不切实际或容易出错。

## 如何操作：

关于使用 Bash 就地编辑文件，有两个突出的工具：`sed` 和 `awk`。让我们探索如何用一些编码示例使用这些强大的工具。

### 使用 `sed` 进行简单文本替换

以下命令在 `file.txt` 中替换第一次出现的 "text1" 为 "text2"：

```Bash
sed -i 's/text1/text2/' file.txt
```

要进行全局替换（所有出现的地方），你需要在末尾添加一个 `g`：

```Bash
sed -i 's/text1/text2/g' file.txt
```

要同时修改多个文件：

```Bash
sed -i 's/text1/text2/g' file1.txt file2.txt file3.txt
```

### 使用 `awk` 进行更复杂的操作

`awk` 是另一个以其编程能力闪耀的工具，特别适用于涉及基于字段的数据的文本处理。

在 `data.csv` 中，以逗号分隔，将每一行的第二个字段更改为 `newValue`：

```Bash
awk -i inplace -F, '{$2="newValue"; print $0}' OFS=, data.csv
```

### 在跳跃之前备份

一个实用的建议：总是在就地编辑之前创建备份。`sed` 通过 `-i` 选项后跟一个后缀来实现备份。

```Bash
sed -i.bak 's/text1/text2/g' file.txt
```

此命令在执行替换之前，会创建原始 `file.txt` 的备份，作为 `file.txt.bak`。

## 深入探讨

直接从命令行编辑文件的能力是 Unix 哲学的自然进展：赋予用户以尽可能少的击键高效管理和操作数据的能力。然而，这种能力也带来了其注意事项。

### 历史背景

像 `sed` 和 `awk` 这样的 Unix 工具自 Unix 初期以来就存在，作为其工具箱哲学的一部分，专注于专业的、可组合的命令。它们被纳入 Unix 武器库是对命令行界面主导的环境中高效文本处理需求的响应。

### 替代方案

尽管 `sed` 和 `awk` 很强大，但它们不是唯一的选项。例如，Perl 和 Python 都有命令行选项（分别是 `-p` 和 `-i`），允许进行类似的就地编辑能力，对于复杂操作来说具有可读性更强的语法。

```Bash
perl -pi -e 's/text1/text2/g' file.txt
```

```Bash
python -c "import fileinput, sys; [sys.stdout.write(line.replace('text1', 'text2')) for line in fileinput.input(files='file.txt', inplace=True)]"
```

每种替代方案都有其优势：Perl 的单行能力极大，而 Python 的语法对于那些不深入了解 Unix 文本处理工具的人来说可能更易于访问。

### 实现细节

技术上来说，就地编辑并不是真正的“就地”，无论是 `sed -i` 还是 `awk -i inplace` 都是通过创建一个临时文件，在替换原文件之前存储处理后的输出。这种方法确保了如果过程被中断，文件不会被损坏。其影响主要在于资源和权限：你必须有足够的磁盘空间来存储临时文件，以及在目标文件的目录中创建文件的权限。

尽管强大，但就地编辑命令必须谨慎使用。一个放置不当的正则表达式可能导致数据丢失，这强调了备份的重要性。尽管有潜在的陷阱，掌握这些命令可以显著增强你直接从命令行进行快速、高效文件修改的能力，体现了利用简单、强大的工具完成复杂任务的 Unix 哲学。
