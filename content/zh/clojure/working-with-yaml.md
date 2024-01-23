---
title:                "处理 YAML 文件"
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
YAML是一种直观的数据序列化格式，用于配置文件和数据交互。程序员使用YAML因为它易读易写，适合处理复杂的数据结构。

## 如何：
Clojure处理YAML需要引入第三方库，如`snakeyaml`。下面是用Clojure读取和解析YAML的示例：

```Clojure
(require '[clojure.java.io :as io])
(require '[clj-yaml.core :as yaml])

; 读取YAML文件
(def yaml-content (slurp (io/resource "config.yaml")))

; 解析YAML内容
(def config-data (yaml/parse-string yaml-content))

; 打印解析后的数据
(println config-data)
```

输出示例：

```Clojure
{:database {:url "jdbc:mysql://localhost:3306/db", 
            :user "root", 
            :password "password123"}}
```

## 深入探索
YAML（YAML Ain't Markup Language）起源于2001年，用于替代复杂的XML。JSON是YAML的一种简化形式，二者通过数据结构非常类似。YAML在Clojure中通过`snakeyaml`库以Java库的形势实现。尽管表面简单，YAML要精通还需注意缩进、数据类型转换等问题。

## 参考链接
- YAML 官方网站: [https://yaml.org](https://yaml.org)
- Clojure YAML库文档: [https://github.com/clj-commons/clj-yaml](https://github.com/clj-commons/clj-yaml)
- YAML和JSON对比: [https://stackoverflow.com/questions/1726802/what-is-the-difference-between-yaml-and-json](https://stackoverflow.com/questions/1726802/what-is-the-difference-between-yaml-and-json)
- Clojure官方文档: [https://clojure.org](https://clojure.org)
