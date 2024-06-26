---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:35.241328-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 b\u1EAFt \u0111\u1EA7u l\xE0m vi\u1EC7\
  c v\u1EDBi TOML trong Go, b\u1EA1n c\u1EA7n ph\u1EA3i bao g\u1ED3m m\u1ED9t th\u01B0\
  \ vi\u1EC7n c\xF3 th\u1EC3 ph\xE2n t\xEDch t\u1EC7p TOML v\xEC th\u01B0 vi\u1EC7\
  n ti\xEAu chu\u1EA9n c\u1EE7a Go kh\xF4ng h\u1ED7 tr\u1EE3\u2026"
lastmod: '2024-03-13T22:44:36.013860-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 b\u1EAFt \u0111\u1EA7u l\xE0m vi\u1EC7c v\u1EDBi TOML trong\
  \ Go, b\u1EA1n c\u1EA7n ph\u1EA3i bao g\u1ED3m m\u1ED9t th\u01B0 vi\u1EC7n c\xF3\
  \ th\u1EC3 ph\xE2n t\xEDch t\u1EC7p TOML v\xEC th\u01B0 vi\u1EC7n ti\xEAu chu\u1EA9\
  n c\u1EE7a Go kh\xF4ng h\u1ED7 tr\u1EE3 TOML m\u1ED9t c\xE1ch t\u1EF1 nhi\xEAn."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

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
