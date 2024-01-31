---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (是什么与为什么？)
处理YAML是关于如何在Swift中读写YAML文件这种人类可读的数据序列化格式。程序员这么做是因为YAML简洁、清晰，适合配置文件、数据交换等。

## How to: (如何操作：)
```Swift
import Foundation
import Yams

let yamlString = """
- name: John Doe
  age: 30
- name: Jane Smith
  age: 25
"""

// 解析YAML字符串
if let people = try? Yams.load(yaml: yamlString) {
    print(people)
}
```
输出：
```Swift
[
  ["name": "John Doe", "age": 30],
  ["name": "Jane Smith", "age": 25]
]
```

## Deep Dive (深入探究)
YAML，即“YAML Ain't Markup Language”，容易与人类和机器通信。1999年出现，JSON和XML是替代品。Swift处理YAML需第三方库，如Yams。实施时关注解析速度、内存管理等。

## See Also (另见)
- YAML官网：[https://yaml.org](https://yaml.org)
- Yams库GitHub页面：[https://github.com/jpsim/Yams](https://github.com/jpsim/Yams)
- Swift编码风格指导：[https://swift.org/documentation/api-design-guidelines/](https://swift.org/documentation/api-design-guidelines/)
