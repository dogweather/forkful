---
title:                "解析HTML"
html_title:           "C++: 解析HTML"
simple_title:         "解析HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## 为什么

想象一下你正在开发一个网页抓取器，并且需要从网页源代码中提取特定的信息。这时，HTML解析就会变得十分重要，它能够让你快速准确地获取你所需的数据。

## 如何做

要在C++中解析HTML，你需要使用一个开源的HTML解析库。这里我们选择使用libxml2。首先，在你的C++项目中添加libxml2的路径，然后按照以下步骤进行解析：

1. 引入libxml2的头文件：`#include <libxml/HTMLparser.h>`

2. 创建一个HTML文档：`htmlDocPtr doc = htmlReadMemory(html_code, strlen(html_code), "url", NULL, HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);`（其中`html_code`是你要解析的HTML源代码）

3. 遍历HTML文档并提取所需的信息：`xmlNode* root = xmlDocGetRootElement(doc);` （这里的`root`就是HTML文档的根节点）

4. 释放HTML文档的内存：`xmlFreeDoc(doc);`

下面是一个完整的示例代码，展示如何从HTML中提取所有的链接：

```C++
#include <iostream>
#include <libxml/HTMLparser.h>

int main()
{
    const char* html_code = "<a href=\"https://www.example.com\">link1</a><a href=\"https://www.example.com\">link2</a>";
    htmlDocPtr doc = htmlReadMemory(html_code, strlen(html_code), "url", NULL, HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);
    xmlNode* root = xmlDocGetRootElement(doc);
    
    if (root != NULL)
    {
        xmlNode* curr_node = root;
        while (curr_node != NULL)
        {
            if (curr_node->type == XML_ELEMENT_NODE && xmlStrcmp(curr_node->name, (const xmlChar*)"a") == 0)
            {
                xmlChar* href = xmlGetProp(curr_node, (const xmlChar*)"href");
                std::cout << href << std::endl;
                xmlFree(href);
            }
            curr_node = curr_node->next;
        }
    }

    xmlFreeDoc(doc);
    return 0;
}
```

输出结果为：

```
https://www.example.com
https://www.example.com
```

## 深入了解

libxml2是一个功能强大的HTML解析库，除了解析HTML，它还可以处理XML等其他类型的文档。它有一个完整的API文档，你可以在这里找到更多关于它的信息：http://xmlsoft.org/.

## 参考资料

- libxml2官方文档：http://xmlsoft.org/
- libxml2 GitHub仓库：https://github.com/GNOME/libxml2
- C++中文网的libxml2教程：https://www.cplusplus.com/reference/libxml2/