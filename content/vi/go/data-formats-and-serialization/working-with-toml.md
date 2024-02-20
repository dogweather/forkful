---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:35.241328-07:00
description: "TOML (Tom's Obvious, Minimal Language - Ng\xF4n ng\u1EEF T\u1ED1i gi\u1EA3\
  n, Hi\u1EC3n nhi\xEAn c\u1EE7a Tom) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1ng t\u1EC7\
  p c\u1EA5u h\xECnh d\u1EC5 \u0111\u1ECDc nh\u1EDD c\xFA ph\xE1p \u0111\u01A1n gi\u1EA3\
  n c\u1EE7a n\xF3. L\u1EADp tr\xECnh\u2026"
lastmod: 2024-02-19 22:04:55.201121
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language - Ng\xF4n ng\u1EEF T\u1ED1i gi\u1EA3\
  n, Hi\u1EC3n nhi\xEAn c\u1EE7a Tom) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1ng t\u1EC7\
  p c\u1EA5u h\xECnh d\u1EC5 \u0111\u1ECDc nh\u1EDD c\xFA ph\xE1p \u0111\u01A1n gi\u1EA3\
  n c\u1EE7a n\xF3. L\u1EADp tr\xECnh\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

TOML (Tom's Obvious, Minimal Language - Ngôn ngữ Tối giản, Hiển nhiên của Tom) là một định dạng tệp cấu hình dễ đọc nhờ cú pháp đơn giản của nó. Lập trình viên sử dụng TOML để cấu hình các thiết lập và phụ thuộc của ứng dụng bởi vì sự rõ ràng và việc ánh xạ trực tiếp đến cấu trúc dữ liệu, khiến nó trở thành lựa chọn phổ biến trong nhiều dự án Go để thiết lập và quản lý cấu hình.

## Làm thế nào:

Để bắt đầu làm việc với TOML trong Go, bạn cần phải bao gồm một thư viện có thể phân tích tệp TOML vì thư viện tiêu chuẩn của Go không hỗ trợ TOML một cách tự nhiên. Gói `BurntSushi/toml` là một lựa chọn phổ biến cho việc này. Đầu tiên, hãy chắc chắn cài đặt nó:

```bash
go get github.com/BurntSushi/toml
```

Dưới đây là một ví dụ đơn giản về cách sử dụng nó. Giả sử bạn có một tệp cấu hình tên là `config.toml` với nội dung sau:

```toml
title = "Ví dụ TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

Bây giờ, bạn cần tạo một cấu trúc Go phản ánh cấu trúc TOML:

```go
package main

import (
    "fmt"
    "github.com/BurntSushi/toml"
)

type Config struct {
    Title    string
    Database Database `toml:"database"`
}

type Database struct {
    Server        string
    Ports         []int
    ConnectionMax int `toml:"connection_max"`
    Enabled       bool
}

func main() {
    var config Config
    if _, err := toml.DecodeFile("config.toml", &config); err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Tiêu đề: %s\n", config.Title)
    fmt.Printf("Máy chủ cơ sở dữ liệu: %s\n", config.Database.Server)
}
```

Kết quả mẫu:

```
Tiêu đề: Ví dụ TOML
Máy chủ cơ sở dữ liệu: 192.168.1.1
```

## Đi sâu vào

TOML được tạo ra bởi Tom Preston-Werner, một trong những đồng sáng lập của GitHub, nhằm mục đích cung cấp một định dạng tệp cấu hình đơn giản, có thể dễ dàng ánh xạ sang bảng băm và có thể hiểu ngay lập tức mà không cần kiến thức trước về định dạng. Điều này trái ngược với JSON hoặc YAML, mặc dù cũng rộng rãi được sử dụng, nhưng có thể kém thân thiện với con người hơn trong các tệp cấu hình vì vấn đề ngoặc, dấu nháy và thụt lề.

Gói `BurntSushi/toml` trong Go là một thư viện mạnh mẽ không chỉ cho phép giải mã mà còn cho phép mã hóa tệp TOML, khiến nó trở thành một lựa chọn đa dạng cho các ứng dụng cần đọc và viết các tệp cấu hình theo định dạng này. Tuy nhiên, nên lưu ý rằng với sự tiến bộ của công nghệ và sự giới thiệu của các phiên bản Go mới, các lựa chọn khác như `pelletier/go-toml` đã xuất hiện, cung cấp hiệu suất cải thiện và các tính năng bổ sung như thao tác cây và hỗ trợ truy vấn.

Mặc dù TOML là một lựa chọn tuyệt vời cho nhiều ứng dụng, tùy thuộc vào sự phức tạp của cấu hình ứng dụng và sở thích cá nhân hay nhóm, các định dạng khác như YAML hay JSON có thể phù hợp hơn, đặc biệt nếu cấu hình yêu cầu cấu trúc dữ liệu phức tạp hơn mà bản chất dài dòng của TOML có thể không mô tả một cách thanh lịch. Tuy nhiên, đối với các cấu hình dễ đọc, dễ chỉnh sửa và dễ hiểu, TOML, kết hợp với hệ thống kiểu mạnh mẽ của Go và các thư viện đã nêu, là một lựa chọn xuất sắc.
