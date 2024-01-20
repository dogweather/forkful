---
title:                "与 csv 的工作"
html_title:           "Go: 与 csv 的工作"
simple_title:         "与 csv 的工作"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/working-with-csv.md"
---

{{< edit_this_page >}}

## 什么是CSV，以及为什么程序员要使用它？

CSV是一种常见的数据交换格式，用于存储和传输表格数据。它由逗号分隔的值组成，因此得名“逗号分隔值”（Comma-Separated Values）。程序员经常使用CSV来处理大量数据，例如导入和导出数据库、生成报告和图表等。

## 如何使用Go处理CSV？

```Go
// 读取CSV文件
file, err := os.Open("data.csv")
if err != nil {
    log.Fatal(err)
}
defer file.Close()

// 使用csv包解析数据
reader := csv.NewReader(file)
records, err := reader.ReadAll()
if err != nil {
    log.Fatal(err)
}

// 打印所有数据行
for _, record := range records {
    fmt.Println(record)
}

// 将数据写入CSV文件
file, err := os.Create("output.csv")
if err != nil {
    log.Fatal(err)
}
defer file.Close()

writer := csv.NewWriter(file)
writer.Write([]string{"Name", "Age", "Country"})
writer.Write([]string{"John", "25", "USA"})
writer.Write([]string{"Maggie", "30", "Canada"})
writer.Flush()
```

输出：

```
[Name Age Country]
[John 25 USA]
[Maggie 30 Canada]
```

## 深入了解CSV

CSV最早于1972年出现，是纺织业用于传输数据的标准格式。它最大的优点是简单易懂，因此得到广泛应用。除了逗号，CSV也可以使用其他符号来分隔值，例如分号、制表符等。在处理CSV时，程序员还可以使用第三方库来提高效率，例如gocsv和gocsv-tag。

## 参考链接

- [Go官方文档：package csv](https://golang.org/pkg/encoding/csv/)
- [gocsv：高性能CSV包](https://github.com/gocarina/gocsv)