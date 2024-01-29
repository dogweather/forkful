---
title:                "Làm việc với XML"
date:                  2024-01-28T22:11:21.238450-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm gì & Tại sao?
Làm việc với XML bao gồm việc phân tích cú pháp, tạo và thao tác với các tài liệu XML bằng cách sử dụng mã lệnh. Các lập trình viên thực hiện điều này để trao đổi dữ liệu, tệp cấu hình và dịch vụ web vì tính dễ đọc và sự hỗ trợ rộng rãi của XML khiến nó trở thành một lựa chọn vững chắc cho dữ liệu có cấu trúc.

## Cách làm:
Trong Go, sử dụng gói `encoding/xml`. Chúng ta hãy phân tích cú pháp và tạo XML.
```go
package main

import (
	"encoding/xml"
	"fmt"
	"os"
)

// Cấu trúc ánh xạ tới các phần tử XML
type Plant struct {
	XMLName xml.Name `xml:"plant"`
	Id      int      `xml:"id,attr"`
	Name    string   `xml:"name"`
	Origin  []string `xml:"origin"`
}

func main() {
	coffee := &Plant{Id: 27, Name: "Coffee"}
	coffee.Origin = []string{"Ethiopia", "Brazil"}

	// Marshal cấu trúc thành XML
	output, err := xml.MarshalIndent(coffee, " ", "  ")
	if err != nil {
		fmt.Printf("Lỗi: %v\n", err)
	}

	os.Stdout.Write([]byte(xml.Header))
	os.Stdout.Write(output)

	// Unmarshal XML thành cấu trúc
	data := `
<plant id="27">
  <name>Coffee</name>
  <origin>Ethiopia</origin>
  <origin>Brazil</origin>
</plant>
`
	var p Plant
	if err := xml.Unmarshal([]byte(data), &p); err != nil {
		fmt.Printf("Lỗi: %v", err)
		return
	}

	fmt.Printf("\n\nGiải mã: %+v", p)
}
```
Kết quả mẫu:
```xml
<?xml version="1.0" encoding="UTF-8"?>
 <plant id="27">
   <name>Coffee</name>
   <origin>Ethiopia</origin>
   <origin>Brazil</origin>
 </plant>

Giải mã: {XMLName:{Space: Local:plant} Id:27 Name:Coffee Origin:[Ethiopia Brazil]}
```

## Sâu hơn
XML đã xuất hiện từ cuối những năm '90, được thiết kế cho việc xuất bản điện tử ào lớn nhưng nhanh chóng được áp dụng cho web. Các lựa chọn thay thế như JSON đã xuất hiện, được ca ngợi vì sự đơn giản, nhưng việc xác thực tài liệu thông qua các kịch bản và không gian tên vẫn mạnh mẽ cho các tài liệu phức tạp. Trong Go, `encoding/xml` xử lý hầu hết các nhiệm vụ, nhưng đối với các tài liệu lớn hoặc xử lý luồng, cân nhắc sử dụng `xml.NewDecoder` và `xml.NewEncoder` để kiểm soát cấp thấp hơn và hiệu suất tốt hơn.

## Xem thêm
- Gói `encoding/xml` của Go: https://pkg.go.dev/encoding/xml
- Hướng dẫn XML: https://www.w3schools.com/xml/
- Blog Go về XML: https://blog.golang.org/xml
- So sánh giữa JSON và XML: https://www.json.org/xml.html
