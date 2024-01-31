---
title:                "使用TOML"
date:                  2024-01-26T04:19:39.706396-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/working-with-toml.md"
---

{{< edit_this_page >}}

## 什么是 TOML 以及为何使用？
TOML 是一个旨在易于读写的数据序列化语言。由于其清晰度和人性化的特性，程序员们使用它来处理配置文件、简单的数据存储以及跨语言的数据交换。

## 如何操作：
让我们使用 "tomlc99" 库在C语言中解析一个 TOML 配置文件。首先，安装该库。然后，创建一个 `config.toml` 文件：

```toml
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

现在，在 C 语言中解析它：

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Error: cannot open config file\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Error: %s\n", errbuf);
        return 1;
    }

    printf("Title: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Owner Name: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
示例输出：
```
Title: "TOML Example"
Owner Name: "Tom Preston-Werner"
```

## 深入了解
TOML，即 Tom's Obvious, Minimal Language 的缩写，由 Tom Preston-Werner 在 2013 年创建。它旨在成为 XML 和 YAML 等格式的更简单替代品，专注于更易于人类阅读和书写。虽然 JSON 是另一种替代品，但 TOML 保留了一种更容易被人类视觉解析的结构，这是其在配置文件中被采纳的主要原因之一。

在 C 语言中，处理 TOML 数据时需要选择一个解析库，因为该语言本身不支持它。像 "tomlc99" 这样的库符合 C99 标准，并提供了一个 API 来解码 TOML 文本。在考虑性能时，适当的错误处理和内存管理至关重要，因为 C 语言没有内置的垃圾收集功能。

## 另请参见：
1. TOML 规范：[https://toml.io/en/](https://toml.io/en/)
2. tomlc99 GitHub 仓库：[https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. 比较数据序列化格式：[https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
