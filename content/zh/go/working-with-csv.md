---
title:                "Go: 使用csv进行编程"
simple_title:         "使用csv进行编程"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/working-with-csv.md"
---

{{< edit_this_page >}}

## 为什么

CSV（逗号分隔值）是一种常见的数据存储格式，它可以用于存储结构化数据，如表格和数据库。在Go编程中，使用CSV可以帮助我们更轻松地处理和操作数据，从而提高我们的效率。

## 如何进行操作

想要在Go中操作CSV，我们首先需要导入"encoding/csv"包。然后，我们可以使用`csv.NewReader()`函数来创建一个读取CSV文件的对象。接着，我们可以使用`.Read()`方法来逐行读取该文件的内容，并将数据存储在一个二维数组中。

```Go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
)

func main() {
	// 创建一个读取CSV文件的对象
	file, err := os.Open("data.csv")
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	defer file.Close()

	reader := csv.NewReader(file)

	// 逐行读取文件内容
	records, err := reader.ReadAll()
	if err != nil {
		fmt.Println("Error:", err)
		return
	}

	// 将数据存储在二维数组中
	var data [][]string
	for _, record := range records {
		data = append(data, record)
	}

	fmt.Println(data)
}
```

上述示例代码会输出类似如下的二维数组：

```
[[John Smith 30 New York]
[Amy Chen 25 Beijing]
[David Lee 28 London]]
```

我们也可以使用`csv.NewWriter()`函数来创建一个写入CSV文件的对象。然后，我们可以使用`.Write()`方法来逐行写入数据并保存到CSV文件中。

```Go
package main

import (
	"encoding/csv"
	"os"
)

func main() {
	// 创建一个写入CSV文件的对象
	file, err := os.Create("output.csv")
	if err != nil {
		fmt.Println("Error:", err)
		return
	}
	defer file.Close()

	writer := csv.NewWriter(file)

	// 写入数据到CSV文件中
	data := [][]string{{"Name", "Age", "Location"}, {"John Smith", "30", "New York"}, {"Amy Chen", "25", "Beijing"}, {"David Lee", "28", "London"}}
	writer.WriteAll(data)
	writer.Flush()
}
```

这样就会在当前目录下创建一个名为"output.csv"的文件，并将数据写入其中。

## 深入了解

除了上述基本的读写操作外，我们还可以使用`csv.Reader`和`csv.Writer`的其他方法来处理CSV文件。例如，我们可以设置`csv.Reader`的`.Comma`属性来指定不同的分隔符，比如制表符、冒号等。我们还可以使用`.TrimLeadingSpace`属性来去除读取的数据中的空格。

此外，我们可以使用`csv.Reader`的`.ReadHeader()`方法来读取CSV文件的第一行作为表头，从而方便我们直接操作数据。我们也可以使用`csv.Writer`的`.UseCRLF`属性来指定换行符为CRLF，从而在Windows系统上正确地写入CSV文件。

## 查看更多

- 官方Go文档中的CSV包说明：https://golang.org/pkg/encoding/csv/
- go-csv项目：https://github.com/go-ozzo/ozzo-io/tree/master/csv 
- 使用标准库而不是第三方包来处理CSV文件：https://blog.golang.org/csv