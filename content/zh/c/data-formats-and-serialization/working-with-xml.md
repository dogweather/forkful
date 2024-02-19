---
aliases:
- /zh/c/working-with-xml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:04.966334-07:00
description: "\u5728 C \u4E2D\u5904\u7406 XML \u6D89\u53CA\u4F7F\u7528\u5404\u79CD\
  \u5E93\u6765\u89E3\u6790\u3001\u67E5\u8BE2\u548C\u64CD\u4F5C XML \u6587\u6863\u3002\
  \u7A0B\u5E8F\u5458\u4E4B\u6240\u4EE5\u4F7F\u7528 XML\uFF0C\u662F\u56E0\u4E3A\u5B83\
  \u5728 Web \u670D\u52A1\u3001\u914D\u7F6E\u6587\u4EF6\u4EE5\u53CA\u4E0D\u540C\u7CFB\
  \u7EDF\u4E4B\u95F4\u7684\u6570\u636E\u4EA4\u6362\u4E2D\u5E7F\u6CDB\u4F7F\u7528\uFF0C\
  \u8FD9\u9700\u8981\u6280\u80FD\u9AD8\u6548\u5730\u5904\u7406 XML \u4EE5\u4FBF\u4E8E\
  \u5065\u58EE\u7684\u5E94\u7528\u7A0B\u5E8F\u5F00\u53D1\u3002"
lastmod: 2024-02-18 23:08:59.583397
model: gpt-4-0125-preview
summary: "\u5728 C \u4E2D\u5904\u7406 XML \u6D89\u53CA\u4F7F\u7528\u5404\u79CD\u5E93\
  \u6765\u89E3\u6790\u3001\u67E5\u8BE2\u548C\u64CD\u4F5C XML \u6587\u6863\u3002\u7A0B\
  \u5E8F\u5458\u4E4B\u6240\u4EE5\u4F7F\u7528 XML\uFF0C\u662F\u56E0\u4E3A\u5B83\u5728\
  \ Web \u670D\u52A1\u3001\u914D\u7F6E\u6587\u4EF6\u4EE5\u53CA\u4E0D\u540C\u7CFB\u7EDF\
  \u4E4B\u95F4\u7684\u6570\u636E\u4EA4\u6362\u4E2D\u5E7F\u6CDB\u4F7F\u7528\uFF0C\u8FD9\
  \u9700\u8981\u6280\u80FD\u9AD8\u6548\u5730\u5904\u7406 XML \u4EE5\u4FBF\u4E8E\u5065\
  \u58EE\u7684\u5E94\u7528\u7A0B\u5E8F\u5F00\u53D1\u3002"
title: "\u5904\u7406XML"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在 C 中处理 XML 涉及使用各种库来解析、查询和操作 XML 文档。程序员之所以使用 XML，是因为它在 Web 服务、配置文件以及不同系统之间的数据交换中广泛使用，这需要技能高效地处理 XML 以便于健壮的应用程序开发。

## 如何操作:

C 没有内置对 XML 的支持，所以你需要使用外部库。一个受欢迎的选择是 `libxml2`，一个稳定且功能丰富的库。以下是使用 `libxml2` 读取和解析 XML 文件的方法。

首先，确保你的系统上安装了 `libxml2`。你可能需要通过包管理器安装它（例如，在 Debian 系统上使用 `apt-get install libxml2-dev`）。

接下来，在你的 C 程序中包含 `libxml2` 的头文件:

```c
#include <libxml/parser.h>
#include <libxml/tree.h>
```

现在，让我们编写一个简单的程序来解析 XML 文件并打印出第一级元素的名称:

```c
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main(void) {
    xmlDoc *document = NULL;
    xmlNode *root_element = NULL;

    // 初始化库并检查潜在的 ABI 不匹配
    LIBXML_TEST_VERSION

    // 解析文件并获取 DOM
    document = xmlReadFile("your_file.xml", NULL, 0);

    if (document == NULL) {
        printf("解析 XML 文件失败\n");
        return -1;
    }

    // 获取根元素节点
    root_element = xmlDocGetRootElement(document);

    for (xmlNode *currentNode = root_element; currentNode; currentNode = currentNode->next) {
        if (currentNode->type == XML_ELEMENT_NODE) {
            printf("节点类型: 元素, 名称: %s\n", currentNode->name);
        }
    }

    // 释放为解析器和 DOM 分配的内存
    xmlFreeDoc(document);

    // 清理和检查泄露
    xmlCleanupParser();
    xmlMemoryDump(); // 可选的

    return 0;
}
```

要编译这个程序，请确保链接 `libxml2`:

```sh
gcc -o xml_example xml_example.c $(xml2-config --cflags --libs)
```

假设你有一个名为 `your_file.xml` 的 XML 文件，运行编译后的程序应该会打印出其第一级元素的名称。

## 深入探索

C 与 XML 的交互是将两个截然不同的世界结合在一起的故事：C 的结构化、字节级、过程化范式与 XML 的层次化、冗长和以文档为中心的模型。在将 XML 处理功能集成到 C 程序中时，开发者利用 C 的优势 —— 如速度和低级内存访问 —— 来高效地解析和操作 XML 文档。

`libxml2` 作为 GNOME 项目的一部分开发，由于其对 XML 标准的全面支持和其性能，成为了 C 中 XML 处理的事实标准。它体现了多年的开发努力和社区贡献，使其成为大多数 XML 任务的强大且高效工具。

虽然 `libxml2` 提供了强大的功能，但值得注意的是，XML 解析和操作的复杂性可能会引入重大开销。在 XML 的冗长和复杂性无法证明其合理性的情景中，像 JSON 这样的替代品可能更适合数据交换。然而，对于以 XML 为中心的应用程序或 XML 使用根深蒂固的环境，掌握在 C 中 `libxml2` 的使用解锁了处理广泛 XML 文档和 API 的能力，弥合了 C 编程语言与结构化文档处理世界之间的差距。
