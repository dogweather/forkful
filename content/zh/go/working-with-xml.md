---
title:                "处理XML"
date:                  2024-01-26T04:31:58.139845-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/working-with-xml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
使用 XML 处理涉及解析、创建和使用代码操作 XML 文档。程序员之所以这样做，是为了数据交换、配置文件和 Web 服务，因为 XML 的可读性和广泛支持使其成为结构化数据的坚实选择。

## 如何操作:
在 Go 中，使用 `encoding/xml` 包。我们来解析和生成 XML。
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// 结构体映射到 XML 元素
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Ethiopia", "Brazil"}

	// 将结构体编组（Marshal）为 XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("错误: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// 将 XML 解组（Unmarshal）为结构体
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Ethiopia</origin>
  <origin>Brazil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("错误: %v", err)
		return
	}

	fmt.Printf("\n\n解组结果: %+v", p)
}
```
示例输出:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Ethiopia</origin>
   <origin>Brazil</origin>
 </plant>

解组结果: {XMLName:{Space: Local:plant} Id:27 Name:Coffee Origin:[Ethiopia Brazil]}
```

## 深入了解
XML 从 90 年代末就开始使用，最初设计用于大规模电子出版，但很快就被 Web 采纳。像 JSON 这样的替代方案因其简洁性而崛起，但 XML 通过模式和命名空间进行的文档验证，对于复杂文档依然是强大的。在 Go 中，`encoding/xml` 能处理大多数任务，但对于庞大的文档或流处理，考虑使用 `xml.NewDecoder` 和 `xml.NewEncoder` 获得更低级别的控制和更好的性能。

## 另请参阅
- Go 的 `encoding/xml` 包: https://pkg.go.dev/encoding/xml
- XML 教程: https://www.w3schools.com/xml/
- Go 关于 XML 的博客: https://blog.golang.org/xml
- JSON 与 XML 的对比: https://www.json.org/xml.html
