---
title:                "处理 CSV 文件"
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
处理CSV就是读写逗号分隔值 (CSV) 文件。程序员这么做是因为CSV格式普遍用于数据交换，简单且易于人类阅读和机器解析。

## How to: (怎么做：)
在Clojure中，我们使用`clojure.data.csv`库来处理CSV文件。

```Clojure
;; 导入库
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

;; 读取CSV文件
(with-open [reader (io/reader "data.csv")]
  (doall (csv/read-csv reader)))

;; 写入CSV文件
(with-open [writer (io/writer "out.csv")]
  (csv/write-csv writer [["name" "age"] ["Alice" "30"] ["Bob" "25"]]))
```

输出 `data.csv` 的内容和`out.csv`被成功写入。

## Deep Dive (深入探索)
历史上，CSV文件因其文本形式和多功能性在数据库和电子表格程序间交换数据中很受欢迎。而现在JSON和XML格式常作为替代方案。在Clojure中，处理CSV文件通常更简单且性能良好，但可能需要处理引号、换行等复杂性。

## See Also (另请参阅)
- Clojure官方文档: [Clojure Docs](https://clojure.org/)
- `clojure.data.csv`库: [clojure.data.csv](https://github.com/clojure/data.csv)
