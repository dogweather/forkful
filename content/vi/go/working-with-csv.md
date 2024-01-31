---
title:                "Làm việc với CSV"
date:                  2024-01-28T22:10:30.091149-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?
Làm việc với CSV trong lập trình nghĩa là đọc từ và viết vào các tệp giá trị được phân tách bằng dấu phẩy - một định dạng lưu trữ dữ liệu bảng đơn giản ở dạng văn bản thuần. Lập trình viên sử dụng nó bởi vì nó được hỗ trợ rộng rãi, dễ tạo và giải thích, và đơn giản để nhập vào cơ sở dữ liệu và các chương trình bảng tính.

## Cách thực hiện:
### Đọc một tệp CSV:
```Go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
)

func main() {
	file, err := os.Open("data.csv")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	reader := csv.NewReader(file)
	records, err := reader.ReadAll()
	if err != nil {
		panic(err)
	}

	for _, record := range records {
		fmt.Println(record)
	}
}
```
### Viết vào một tệp CSV:
```Go
package main

import (
	"encoding/csv"
	"os"
)

func main() {
	records := [][]string{
		{"Name", "Age", "City"},
		{"Alice", "25", "New York"},
		{"Bob", "30", "San Francisco"},
	}

	file, err := os.Create("output.csv")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	writer := csv.NewWriter(file)
	defer writer.Flush()

	for _, record := range records {
		if err := writer.Write(record); err != nil {
			panic(err)
		}
	}
}
```

## Sâu hơn nữa
Định dạng CSV đã tồn tại từ đầu những năm 1970, bắt nguồn từ trình biên dịch IBM Fortran (cấp độ G). Mặc dù các định dạng như JSON hoặc XML có thể cung cấp nhiều tính năng và phức tạp hơn, CSV vẫn giữ vững vị thế nhờ vào sự đơn giản tuyệt vời. Trong Go, gói `encoding/csv` xử lý việc phân tích cú pháp và tuần tự hóa CSV. Gói này hỗ trợ tùy chỉnh, chẳng hạn như thiết lập các dấu phân cách trường khác nhau hoặc xử lý số lượng trường biến đổi mỗi bản ghi. Mặc dù nó không xử lý mọi biến thể CSV, nhưng nó làm việc xuất sắc với các định dạng CSV tiêu chuẩn.

## Xem thêm
Để biết thêm về làm việc với CSV trong Go, hãy xem những tài nguyên này:
- Tài liệu chính thức của Go cho gói [`csv`](https://pkg.go.dev/encoding/csv).
- Xem [Go by Example](https://gobyexample.com/reading-files) để biết thêm các thủ tục đọc và viết tệp.
