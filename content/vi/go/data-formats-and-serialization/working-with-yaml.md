---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:10.007732-07:00
description: "L\xE0m Th\u1EBF N\xE0o: \u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi YAML\
  \ trong Go, b\u1EA1n s\u1EBD c\u1EA7n ph\u1EA3i nh\u1EADp m\u1ED9t th\u01B0 vi\u1EC7\
  n h\u1ED7 tr\u1EE3 ph\xE2n t\xEDch v\xE0 xu\u1EA5t nh\u1EADp d\u1EEF li\u1EC7u YAML\
  \ v\xEC th\u01B0 vi\u1EC7n chu\u1EA9n c\u1EE7a Go kh\xF4ng\u2026"
lastmod: '2024-03-13T22:44:36.009930-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi YAML trong Go, b\u1EA1n s\u1EBD c\u1EA7\
  n ph\u1EA3i nh\u1EADp m\u1ED9t th\u01B0 vi\u1EC7n h\u1ED7 tr\u1EE3 ph\xE2n t\xED\
  ch v\xE0 xu\u1EA5t nh\u1EADp d\u1EEF li\u1EC7u YAML v\xEC th\u01B0 vi\u1EC7n chu\u1EA9\
  n c\u1EE7a Go kh\xF4ng bao g\u1ED3m h\u1ED7 tr\u1EE3 tr\u1EF1c ti\u1EBFp cho YAML."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Làm Thế Nào:
Để làm việc với YAML trong Go, bạn sẽ cần phải nhập một thư viện hỗ trợ phân tích và xuất nhập dữ liệu YAML vì thư viện chuẩn của Go không bao gồm hỗ trợ trực tiếp cho YAML. Thư viện phổ biến nhất dành cho mục đích này là "gopkg.in/yaml.v3". Dưới đây là cách bắt đầu:

1. **Cài đặt gói YAML:**

```bash
go get gopkg.in/yaml.v3
```

2. **Phân tích YAML vào một struct Go:**

Đầu tiên, định nghĩa một struct trong Go khớp với cấu trúc của dữ liệu YAML của bạn.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: secret
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("User: %s\nPassword: %s\n", config.Database.User, config.Database.Password)
}
```

**Đầu Ra Mẫu:**

```
User: admin
Password: secret
```

3. **Chuẩn bị một struct Go về dạng YAML:**

Dưới đây là cách chuyển đổi một struct Go trở lại thành YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct {
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      User:     "admin",
      Password: "supersecret",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**Đầu Ra Mẫu:**

```yaml
---
database:
  user: admin
  password: supersecret
```

## Sâu Hơn:
Việc sử dụng YAML trong phát triển phần mềm đã phát triển do định dạng có thể đọc được bởi con người, làm cho nó trở thành lựa chọn lý tưởng cho các tệp cấu hình, tài liệu, hoặc định dạng trao đổi dữ liệu. So với JSON, đối tác của nó, YAML cung cấp bình luận, các loại vô hình, và tính năng quan hệ, cung cấp một khuôn khổ xuất nhập dữ liệu giàu có hơn. Tuy nhiên, sự linh hoạt và các tính năng của nó đi kèm với giá thành là sự phức tạp trong việc phân tích, dẫn đến các rủi ro về an ninh khi không được xử lý cẩn thận (ví dụ, thực hiện mã tùy ý).

Thư viện "gopkg.in/yaml.v3" cho Go là một giải pháp mạnh mẽ cho việc xử lý YAML, tạo ra sự cân bằng giữa sự dễ sử dụng và hỗ trợ tính năng toàn diện. Tính đến thời điểm hiện tại, mặc dù có các lựa chọn thay thế như "go-yaml/yaml" (thư viện đứng sau "gopkg.in/yaml.v3"), phiên bản được chọn thường phụ thuộc vào yêu cầu cụ thể của dự án hoặc sở thích cá nhân. Khi xử lý các bộ dữ liệu lớn hoặc các ứng dụng quan trọng về hiệu suất, lập trình viên có thể cân nhắc các định dạng đơn giản hơn như JSON vì thời gian phân tích và bộ nhớ chiếm dụng thấp hơn. Tuy nhiên, đối với các tệp cấu hình hoặc cài đặt nơi độ dễ đọc và dễ sử dụng của con người là quan trọng nhất, YAML vẫn là một lựa chọn mạnh mẽ trong hệ sinh thái Go.
