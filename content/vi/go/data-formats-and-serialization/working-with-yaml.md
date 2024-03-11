---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:10.007732-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi YAML trong Go bao g\u1ED3m vi\u1EC7c ph\xE2\
  n t\xEDch t\u1EC7p YAML (YAML Ain't Markup Language) - m\u1ED9t chu\u1EA9n h\xF3\
  a xu\u1EA5t nh\u1EADp li\u1EC7u th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi,\
  \ th\xE0nh c\xE1c\u2026"
lastmod: '2024-03-11T00:14:09.234850-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi YAML trong Go bao g\u1ED3m vi\u1EC7c ph\xE2n t\xED\
  ch t\u1EC7p YAML (YAML Ain't Markup Language) - m\u1ED9t chu\u1EA9n h\xF3a xu\u1EA5\
  t nh\u1EADp li\u1EC7u th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi, th\xE0nh\
  \ c\xE1c\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Làm việc với YAML trong Go bao gồm việc phân tích tệp YAML (YAML Ain't Markup Language) - một chuẩn hóa xuất nhập liệu thân thiện với con người, thành các cấu trúc dữ liệu của Go và ngược lại. Lập trình viên làm điều này để tận dụng sự đơn giản và dễ đọc của YAML cho các tệp cấu hình, cài đặt ứng dụng, hoặc trao đổi dữ liệu giữa các dịch vụ và thành phần được viết bằng các ngôn ngữ khác nhau.

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
