---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:36.826884-07:00
description: "YAML\uFF0C\u4EE3\u8868\u201CYAML \u4E0D\u662F\u6807\u8BB0\u8BED\u8A00\
  \u201D\uFF08YAML Ain't Markup Language\uFF09\uFF0C\u662F\u4E00\u79CD\u4EBA\u7C7B\
  \u53EF\u8BFB\u7684\u6570\u636E\u5E8F\u5217\u5316\u6807\u51C6\uFF0C\u53EF\u7528\u4E8E\
  \u4ECE\u914D\u7F6E\u6587\u4EF6\u5230\u6570\u636E\u5B58\u50A8\u7684\u5404\u79CD\u5E94\
  \u7528\u3002\u5F53\u7A0B\u5E8F\u5458\u9700\u8981\u4E00\u79CD\u6613\u4E8E\u9605\u8BFB\
  \u548C\u7F16\u5199\u7684\u683C\u5F0F\u6765\u5904\u7406\u914D\u7F6E\u6587\u4EF6\u6216\
  \u5728\u8BED\u8A00\u548C\u7CFB\u7EDF\u4E4B\u95F4\u4EA4\u6362\u6570\u636E\u65F6\uFF0C\
  \u4ED6\u4EEC\u901A\u5E38\u4F1A\u4F7F\u7528YAML\u3002"
lastmod: '2024-02-25T18:49:45.903037-07:00'
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u4EE3\u8868\u201CYAML \u4E0D\u662F\u6807\u8BB0\u8BED\u8A00\u201D\
  \uFF08YAML Ain't Markup Language\uFF09\uFF0C\u662F\u4E00\u79CD\u4EBA\u7C7B\u53EF\
  \u8BFB\u7684\u6570\u636E\u5E8F\u5217\u5316\u6807\u51C6\uFF0C\u53EF\u7528\u4E8E\u4ECE\
  \u914D\u7F6E\u6587\u4EF6\u5230\u6570\u636E\u5B58\u50A8\u7684\u5404\u79CD\u5E94\u7528\
  \u3002\u5F53\u7A0B\u5E8F\u5458\u9700\u8981\u4E00\u79CD\u6613\u4E8E\u9605\u8BFB\u548C\
  \u7F16\u5199\u7684\u683C\u5F0F\u6765\u5904\u7406\u914D\u7F6E\u6587\u4EF6\u6216\u5728\
  \u8BED\u8A00\u548C\u7CFB\u7EDF\u4E4B\u95F4\u4EA4\u6362\u6570\u636E\u65F6\uFF0C\u4ED6\
  \u4EEC\u901A\u5E38\u4F1A\u4F7F\u7528YAML\u3002"
title: "\u4F7F\u7528YAML\u8FDB\u884C\u7F16\u7A0B"
---

{{< edit_this_page >}}

## 什么和为什么？

YAML，代表“YAML 不是标记语言”（YAML Ain't Markup Language），是一种人类可读的数据序列化标准，可用于从配置文件到数据存储的各种应用。当程序员需要一种易于阅读和编写的格式来处理配置文件或在语言和系统之间交换数据时，他们通常会使用YAML。

## 如何操作：

在C语言中处理YAML需要一个库，因为标准C库不直接支持YAML解析或序列化。对于C语言，最受欢迎的YAML库之一是`libyaml`，它为解析和生成YAML提供了低级和高级接口。以下是使用`libyaml`解析一个简单YAML文件的示例：

**首先**，你需要安装`libyaml`库。如果你使用的是类Unix系统，通常可以通过包管理器安装。例如，在Ubuntu上：

```bash
sudo apt-get install libyaml-dev
```

**接下来**，考虑一个名为`config.yaml`的简单YAML文件：

```yaml
name: John Doe
age: 29
married: false
```

**这里**是如何在C语言中解析这个YAML文件的基础示例：

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("Failed to initialize YAML parser!\n", stderr);

    if (fh == NULL)
        fputs("Failed to open file!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Value: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

这个简单的程序打开一个YAML文件，初始化YAML解析器，并读取文件，打印标量值（在此示例中，为我们简单YAML的字段）。注意，在这个简单示例中，错误检查很少，并且在生产代码中应该更加健壮。

使用我们的`config.yaml`运行程序将输出：

```plaintext
Value: John Doe
Value: 29
Value: false
```

## 深入探讨

YAML于2001年首次发布，旨在比其他数据序列化格式如XML或JSON更易于阅读和使用友好，借鉴了C、Perl和Python等多种语言的设计哲学。尽管在可读性和易于人工修改的方面具有优势，但由于YAML依赖缩进并且功能集广泛（包括引用和自定义类型），因此从程序角度解析YAML可能相对复杂。

虽然`libyaml`为在C语言中解析和生成YAML提供了健壮的低级访问能力，但由于其API冗长，对于简单任务来说可能会显得笨重。因此，出于这些原因，一些程序员更喜欢使用高级库，或者即使在C语言中工作时也使用其他数据序列化格式如JSON，特别是当需要性能高效解析且代码开销最小是优先级时。然而，YAML在配置文件和需要人类可读性的情况下仍然是受欢迎的选择。选择像TinyYAML或嵌入高级解释器（例如，嵌入Python或Lua）可能为特定应用提供更多便利性，平衡使用便利性和性能需求之间的关系。
