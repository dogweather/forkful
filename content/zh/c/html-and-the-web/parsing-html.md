---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:55.821402-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u7531\u4E8E HTML \u7684\u590D\u6742\u6027\
  \u53CA\u5176\u7ECF\u5E38\u504F\u79BB\u5E72\u51C0\u3001\u826F\u597D\u7684\u7ED3\u6784\
  \uFF0C\u89E3\u6790 HTML \u4F3C\u4E4E\u662F\u4E00\u4E2A\u8270\u5DE8\u7684\u4EFB\u52A1\
  \u3002\u7136\u800C\uFF0C\u4F7F\u7528\u4E00\u4E2A\u5E93\uFF0C\u6BD4\u5982 `libxml2`\uFF0C\
  \u7279\u522B\u662F\u5B83\u7684 HTML \u89E3\u6790\u6A21\u5757\uFF0C\u53EF\u4EE5\u7B80\
  \u5316\u8FD9\u4E2A\u8FC7\u7A0B\u3002\u8FD9\u4E2A\u793A\u4F8B\u6F14\u793A\u4E86\u5982\
  \u4F55\u4F7F\u7528 `libxml2` \u6765\u89E3\u6790 HTML \u5E76\u63D0\u53D6\u4FE1\u606F\
  \u3002 \u9996\u5148\uFF0C\u786E\u4FDD `libxml2`\u2026"
lastmod: '2024-04-05T22:38:47.453718-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u7531\u4E8E HTML \u7684\u590D\u6742\u6027\
  \u53CA\u5176\u7ECF\u5E38\u504F\u79BB\u5E72\u51C0\u3001\u826F\u597D\u7684\u7ED3\u6784\
  \uFF0C\u89E3\u6790 HTML \u4F3C\u4E4E\u662F\u4E00\u4E2A\u8270\u5DE8\u7684\u4EFB\u52A1\
  \u3002\u7136\u800C\uFF0C\u4F7F\u7528\u4E00\u4E2A\u5E93\uFF0C\u6BD4\u5982 `libxml2`\uFF0C\
  \u7279\u522B\u662F\u5B83\u7684 HTML \u89E3\u6790\u6A21\u5757\uFF0C\u53EF\u4EE5\u7B80\
  \u5316\u8FD9\u4E2A\u8FC7\u7A0B\u3002\u8FD9\u4E2A\u793A\u4F8B\u6F14\u793A\u4E86\u5982\
  \u4F55\u4F7F\u7528 `libxml2` \u6765\u89E3\u6790 HTML \u5E76\u63D0\u53D6\u4FE1\u606F\
  \u3002 \u9996\u5148\uFF0C\u786E\u4FDD `libxml2` \u5DF2\u7ECF\u5728\u4F60\u7684\u73AF\
  \u5883\u4E2D\u5B89\u88C5\u3002\u5728\u8BB8\u591A Linux \u53D1\u884C\u7248\u4E2D\uFF0C\
  \u4F60\u53EF\u4EE5\u901A\u8FC7\u5305\u7BA1\u7406\u5668\u5B89\u88C5\u3002\u4F8B\u5982\
  \uFF0C\u5728 Ubuntu \u4E0A\uFF1A."
title: "\u89E3\u6790HTML"
weight: 43
---

## 如何操作：
由于 HTML 的复杂性及其经常偏离干净、良好的结构，解析 HTML 似乎是一个艰巨的任务。然而，使用一个库，比如 `libxml2`，特别是它的 HTML 解析模块，可以简化这个过程。这个示例演示了如何使用 `libxml2` 来解析 HTML 并提取信息。

首先，确保 `libxml2` 已经在你的环境中安装。在许多 Linux 发行版中，你可以通过包管理器安装。例如，在 Ubuntu 上：

```bash
sudo apt-get install libxml2 libxml2-dev
```

现在，让我们编写一个简单的 C 程序，使用 `libxml2` 来解析一个 HTML 字符串，并打印出一个特定元素内的文本：

```c
#include <stdio.h>
#include <libxml/HTMLparser.h>

void parseHTML(const char *html) {
    htmlDocPtr doc = htmlReadDoc((const xmlChar *)html, NULL, NULL, HTML_PARSE_RECOVER | HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);
    
    // 假如我们寻找 <p> 标签内的内容
    xmlNode *root_element = xmlDocGetRootElement(doc);
    for (xmlNode *current_node = root_element; current_node; current_node = current_node->next) {
        if (current_node->type == XML_ELEMENT_NODE && strcmp((const char *)current_node->name, "p") == 0) {
            printf("发现段落: %s\n", xmlNodeGetContent(current_node));
        }
    }
    
    xmlFreeDoc(doc);
    xmlCleanupParser();
}

int main() {
    const char *html = "<html><body><p>Hello, world!</p></body></html>";
    parseHTML(html);
    return 0;
}
```

示例输出：
```
发现段落: Hello, world!
```

这个示例着重于提取段落标签内的文本，但 `libxml2` 为导航和查询 HTML 文档的各个部分提供了强大的支持。

## 深入探讨
在 C 中解析 HTML 可追溯到网络开发的早期。最初，由于缺乏标准化的库和网络上 HTML 的混乱状态，开发者不得不依赖自制的、通常是原始的解析方案。像 `libxml2` 这样的库的引入标志着一个重要进展，提供了更标准化、高效和弹性的 HTML 解析方法。

尽管 C 拥有无与伦比的速度和控制能力，但值得注意的是，C 可能并不总是解析 HTML 的最佳工具，特别是对于需要快速开发周期或处理特别格式不正确的 HTML 的任务。提供更高级、用户友好接口的高级 HTML 解析库语言，如 Python 与 Beautiful Soup，以一些性能为代价。

然而，对于性能至关重要的应用，或当操作在资源有限的环境中时，用 C 解析 HTML 仍然是一个可行且常被首选的方法。关键是利用像 `libxml2` 这样的强大库来处理 HTML 的复杂性，允许开发人员专注于提取他们需要的数据，而不必深陷解析机制的细节之中。
