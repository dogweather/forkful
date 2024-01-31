---
title:                "Làm việc với TOML"
date:                  2024-01-28T22:11:12.061254-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với TOML"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Làm việc với TOML bao gồm việc phân tích cú pháp và mã hóa các tệp TOML (Tom's Obvious, Minimal Language) trong Go. Lập trình viên lựa chọn TOML vì tính dễ đọc và dễ ánh xạ vào các cấu trúc dữ liệu, phù hợp với cấu hình.

## Làm thế nào:
Để làm việc với TOML trong Go, bạn thường sẽ sử dụng một thư viện như `BurntSushi/toml`. Dưới đây là cái nhìn nhanh về việc phân tích cú pháp một tệp cấu hình TOML:

```Go
package main

import (
    "fmt"
    "os"

    "github.com/BurntSushi/toml"
)

type Config struct {
    Title   string
    Owner   struct {
        Name string
    }
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Title: %s, Owner: %s\n", config.Title, config.Owner.Name)
}
```

Mẫu `config.toml`:

```Toml
title = "Ví dụ TOML"
[owner]
name = "Tom Preston-Werner"
```

Mẫu đầu ra:

```
Title: Ví dụ TOML, Owner: Tom Preston-Werner
```

## Sâu hơn
TOML, được giới thiệu bởi Tom Preston-Werner vào năm 2013, được thiết kế để là một định dạng tệp cấu hình tối thiểu, dễ đọc do ngữ nghĩa rõ ràng của nó. Các nhà phát triển Go thường sử dụng TOML cho cấu hình thay cho các lựa chọn khác như JSON hoặc YAML vì tính trực tiếp và khả năng biểu đạt các hệ thống phân cấp phức tạp một cách đơn giản.

So với YAML, có các tính năng phức tạp và các vấn đề an ninh tiềm ẩn, thiết kế phẳng của TOML giảm thiểu độ phức tạp và lỗi do đánh máy. Và không giống như JSON, TOML hỗ trợ bình luận, làm cho việc giải thích cấu hình in-line dễ dàng hơn.

Khi làm việc với TOML trong Go, có những điểm tinh tế cần xem xét. Các thẻ cấu trúc có thể tùy chỉnh cách ánh xạ cấu trúc của bạn vào các cấu trúc TOML, và bạn cũng nên nhận thức về cách TOML mảng và bảng nội tuyến được phân tích cú pháp thành các lát và bản đồ Go.

## Xem thêm
- Đặc tả TOML: https://toml.io/en/
- Thư viện BurntSushi/toml: https://github.com/BurntSushi/toml
- So sánh các định dạng tệp cấu hình: https://www.redhat.com/sysadmin/yaml-toml-json-differences
