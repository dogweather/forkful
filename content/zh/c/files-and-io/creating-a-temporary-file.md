---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:18.791794-07:00
description: "\u5982\u4F55\u505A\uFF1A \u5728C\u7F16\u7A0B\u8BED\u8A00\u4E2D\u521B\
  \u5EFA\u4E00\u4E2A\u4E34\u65F6\u6587\u4EF6\u53EF\u4EE5\u5229\u7528\u5982 `tmpfile()`\
  \ \u548C `mkstemp()` \u8FD9\u6837\u7684\u51FD\u6570\u3002 **\u4F7F\u7528 `tmpfile()`**\uFF1A\
  \u6B64\u51FD\u6570\u521B\u5EFA\u4E00\u4E2A\u552F\u4E00\u7684\u4E34\u65F6\u6587\u4EF6\
  \uFF0C\u8BE5\u6587\u4EF6\u4F1A\u5728\u7A0B\u5E8F\u7EC8\u6B62\u6216\u6587\u4EF6\u5173\
  \u95ED\u65F6\u81EA\u52A8\u5220\u9664\u3002"
lastmod: '2024-04-05T22:38:47.482022-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u505A\uFF1A \u5728C\u7F16\u7A0B\u8BED\u8A00\u4E2D\u521B\u5EFA\
  \u4E00\u4E2A\u4E34\u65F6\u6587\u4EF6\u53EF\u4EE5\u5229\u7528\u5982 `tmpfile()` \u548C\
  \ `mkstemp()` \u8FD9\u6837\u7684\u51FD\u6570\u3002 **\u4F7F\u7528 `tmpfile()`**\uFF1A\
  \u6B64\u51FD\u6570\u521B\u5EFA\u4E00\u4E2A\u552F\u4E00\u7684\u4E34\u65F6\u6587\u4EF6\
  \uFF0C\u8BE5\u6587\u4EF6\u4F1A\u5728\u7A0B\u5E8F\u7EC8\u6B62\u6216\u6587\u4EF6\u5173\
  \u95ED\u65F6\u81EA\u52A8\u5220\u9664\u3002"
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
weight: 21
---

## 如何做：
在C编程语言中创建一个临时文件可以利用如 `tmpfile()` 和 `mkstemp()` 这样的函数。

**使用 `tmpfile()`**：此函数创建一个唯一的临时文件，该文件会在程序终止或文件关闭时自动删除。

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp == NULL) {
        perror("创建临时文件失败");
        return 1;
    }

    // 向临时文件写入数据
    fputs("这是一个测试。\n", temp);

    // 回放并读取我们写入的内容
    rewind(temp);
    char buffer[1024];
    while (fgets(buffer, sizeof(buffer), temp) != NULL) {
        printf("%s", buffer);
    }

    // 关闭或程序退出时自动删除
    fclose(temp);

    return 0;
}
```
**示例输出：**
```
这是一个测试。
```

**使用 `mkstemp()`**：提供对临时文件位置及其权限更多的控制。要求使用以 `XXXXXX` 结尾的模板字符串，该函数随后会用一个独特的序列替换这些字符，以防名称冲突。

```c
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
    char template[] = "/tmp/mytemp-XXXXXX";
    int fd = mkstemp(template);

    if (fd == -1) {
        perror("创建临时文件失败");
        return 1;
    }
    
    printf("创建了临时文件：%s\n", template);

    // 使用 mkstemp() 创建的临时文件应手动删除
    unlink(template);

    close(fd);
    return 0;
}
```
**示例输出：**
```
创建了临时文件：/tmp/mytemp-abc123
```

## 深入了解
临时文件的概念并非C特有，而是许多编程环境中的常见功能，因为它在处理短暂数据时十分有用。ISO C标准化的 `tmpfile()` 函数在标准目录中创建一个具有唯一名称的文件，但它的存在是短暂的，这使其非常适合于安全性高或临时操作。

`tmpfile()` 的一个显著限制是它依赖于默认的临时目录，这对所有应用程序来说可能并不合适，特别是在权限或安全性方面。相比之下，`mkstemp()` 允许指定目录，并通过修改提供的模板字符串来确保安全的文件创建，并保证文件名的唯一性，以手动文件管理为代价提供了更多样的解决方案。

然而，创建临时文件可能引入安全漏洞，例如如果处理不当可能出现竞态条件。例如，`tmpfile()` 和 `mkstemp()` 解决了安全创建临时文件的不同方面（自动删除和安全名称生成，分别），但两者都不是万能的。开发者必须考虑他们应用的安全需求细节，包括临时文件引入的潜在漏洞，并可能需要实施额外的保护措施，超出这些函数所提供的。

在更广泛的编程景观中，如内存存储（例如，使用动态数据结构或内存映射文件）可能为临时数据处理提供更好的性能或安全性。然而，物理临时文件在许多场景中仍是一个关键工具，特别是对于大型数据集或涉及进程间通信时。
