---
title:                "Làm việc với YAML"
date:                  2024-01-28T22:11:57.672253-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm việc với YAML trong Go

### Làm gì & Tại sao?
Làm việc với YAML nghĩa là phân tích cú pháp và tạo dữ liệu trong định dạng YAML, một chuẩn hóa lưu trữ dữ liệu dễ đọc cho con người. Lập trình viên thực hiện điều này để quản lý các tệp cấu hình, trao đổi dữ liệu giữa các ngôn ngữ và cấu trúc dữ liệu phức tạp.

### Làm thế nào:
Để làm việc với YAML trong Go, bạn cần một thư viện như `gopkg.in/yaml.v3`. Cài đặt nó sử dụng:

```bash
go get gopkg.in/yaml.v3
```

Dưới đây là cách để phân tích cú pháp YAML:

```Go
package main

import (
	"fmt"
	"log"
	"gopkg.in/yaml.v3"
)

var data = `
a: Dễ dàng!
b:
  c: 2
  d: [3, 4]
`

type StructA struct {
	A string
	B StructB
}

type StructB struct {
	C int
	D []int
}

func main() {
	var s StructA

	err := yaml.Unmarshal([]byte(data), &s)
	if err != nil {
		log.Fatalf("lỗi: %v", err)
	}
	fmt.Println(s)
}
```

Kết quả:

```
{Dễ dàng! {2 [3 4]}}
```

Tạo YAML:

```Go
package main

import (
	"fmt"
	"gopkg.in/yaml.v3"
)

func main() {
	data := StructA{
		A: "Dễ dàng!",
		B: StructB{
			C: 2,
			D: []int{3, 4},
		},
	}

	d, err := yaml.Marshal(&data)
	if err != nil {
		log.Fatalf("lỗi: %v", err)
	}
	fmt.Printf("---\n%s\n", string(d))
}
```

Kết quả:

```
---
a: Dễ dàng!
b:
  c: 2
  d:
  - 3
  - 4
```

### Tìm hiểu sâu
YAML được bắt đầu từ năm 2001, với mục tiêu là một định dạng trao đổi dữ liệu thân thiện với con người. Nó được sử dụng như một phương án thay thế cho JSON và XML vì nó dễ đọc hơn và có thể biểu diễn các cấu trúc dữ liệu phức tạp. Go không có hỗ trợ sẵn cho YAML, do đó các thư viện bên thứ ba như `gopkg.in/yaml.v3` rất phổ biến. Thư viện này bao bọc libyaml, một bộ phân tích cú pháp và phát ra YAML bằng C, để đạt hiệu quả và tuân thủ với các chuẩn YAML.

### Xem thêm
- Tài liệu của gói YAML v3: https://pkg.go.dev/gopkg.in/yaml.v3
- Trang web chính thức của YAML: https://yaml.org
- Đặc tả YAML: https://yaml.org/spec/1.2/spec.html
- Chuyển đổi từ JSON sang YAML trực tuyến: https://www.json2yaml.com/
