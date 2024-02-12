---
title:                "处理JSON数据"
aliases:
- /zh/c/working-with-json.md
date:                  2024-02-03T18:11:53.548631-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理JSON数据"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在C语言中操作JSON（JavaScript对象表示法）包括解析、生成和操纵JSON数据结构。程序员这样做是为了实现与Web服务的通讯、数据存储或以轻量级和人类可读的格式配置文件。

## 如何操作：

由于C语言没有内置对JSON的支持，因此在C中使用JSON，你通常会使用像`jansson`或`json-c`这样的库。这里，我们将关注`jansson`，因为它易用且维护活跃。首先，安装这个库（例如，在Ubuntu上使用包管理器`apt`：`sudo apt-get install libjansson-dev`）。

我们从解析一个JSON字符串并访问其内容开始：

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "error: on line %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("Name: %s\nAge: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

示例输出：
```
Name: John Doe
Age: 30
```

接下来，创建并输出一个JSON对象：

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

示例输出：
```
{"name": "Jane Doe", "age": 25}
```

这些例子展示了加载JSON字符串、解包其值、创建一个新的JSON对象然后将其作为字符串输出的基本操作。

## 深入了解

在C语言中操作JSON的需求来自于Web对JSON作为数据交换的首选格式的采用。JSON的简单性和效率使其迅速超越XML，尽管C最初在直接支持JSON操作方面缺席。早期的解决方案涉及手动字符串操作 - 容易出错且效率低下。诸如`jansson`和`json-c`这样的库应运而生，提供了JSON解析、构建和序列化的健壮API。

虽然`jansson`提供了简单性和易用性，`json-c`可能吸引那些寻求更广泛特性集的人。尽管如此，像C++中的解析库提供了更复杂的抽象，得益于该语言更复杂的数据结构和标准库支持。然而，在C是首选或必需的语言环境中工作时 - 如嵌入式系统或与现有C库接口时 - 使用`jansson`或`json-c`就变得不可或缺了。

值得注意的是，在C语言中处理JSON需要更深入地理解内存管理，因为这些库经常返回需要显式释放的动态分配的对象。这挑战程序员在便利性和防止内存泄漏的责任之间找到平衡，这是编写高效C代码的一个关键方面。
