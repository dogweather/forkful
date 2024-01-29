---
title:                "Làm việc với JSON"
date:                  2024-01-28T22:10:44.546562-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Làm việc với JSON có nghĩa là mã hóa và giải mã dữ liệu trong định dạng JavaScript Object Notation, cách biểu diễn dữ liệu có cấu trúc dựa trên văn bản. Lập trình viên sử dụng nó vì sự đơn giản và phổ biến trong các API web và tệp cấu hình.

## Cách thức:

### Việc biến đổi JSON thành dữ liệu trong Go

```Go
package main

import (
	"encoding/json"
	"fmt"
)

type User struct {
	Name   string `json:"name"`
	Age    int    `json:"age"`
	Active bool   `json:"active"`
}

func main() {
	user := User{Name: "Alice", Age: 25, Active: true}
	jsonData, err := json.Marshal(user)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(jsonData))
}
```

Đầu ra mẫu:
```json
{"name":"Alice","age":25,"active":true}
```

### Việc biến dữ liệu JSON thành dữ liệu trong Go

```Go
package main

import (
	"encoding/json"
	"fmt"
)

func main() {
	var jsonData = []byte(`{"name":"Alice","age":25,"active":true}`)
	user := User{}
	err := json.Unmarshal(jsonData, &user)
	if err != nil {
		panic(err)
	}
	fmt.Printf("%+v\n", user)
}

type User struct {
	Name   string `json:"name"`
	Age    int    `json:"age"`
	Active bool   `json:"active"`
}
```

Đầu ra mẫu:
```
{Name:Alice Age:25 Active:true}
```

## Sâu hơn nữa

JSON, bắt nguồn từ JavaScript, đã trở thành tiêu chuẩn cho việc trao đổi dữ liệu vào giữa những năm 2000. So với XML, nó nhẹ hơn và dễ đọc hơn, đó là lý do tại sao nó là lựa chọn hàng đầu cho các API RESTful. Trong Go, gói `encoding/json` xử lý dữ liệu JSON, sử dụng các thẻ trường của cấu trúc để khớp các khóa JSON với các trường cấu trúc.

Các phương thức thay thế cho JSON bao gồm XML, YAML và các định dạng nhị phân như Protocol Buffers (protobuf). Mỗi định dạng có những trường hợp sử dụng riêng; ví dụ, YAML được ưu tiên cho các tệp cấu hình được viết bởi con người, trong khi protobuf được sử dụng cho việc chuyển dữ liệu được tuần tự hóa hiệu quả, không phụ thuộc vào nền tảng.

Go thực hiện xử lý JSON một cách hiệu quả, tuy nhiên việc sử dụng phản chiếu có thể khiến nó chậm đi so với một số cơ chế tuần tự hóa có thể hoạt động tại thời điểm biên dịch.

## Xem thêm

- Blog của Go về JSON: https://blog.golang.org/json
- Tài liệu gói `encoding/json` của Go: https://pkg.go.dev/encoding/json
- Trang chính thức của JSON về tiêu chuẩn: http://json.org/
