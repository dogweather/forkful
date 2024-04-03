---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:27.240046-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Go, g\xF3i `encoding/json` l\xE0 c\xE1\
  nh c\u1ED5ng c\u1EE7a b\u1EA1n \u0111\u1EBFn v\u1EDBi vi\u1EC7c \u0111i\u1EC1u khi\u1EC3\
  n JSON, cung c\u1EA5p c\u01A1 ch\u1EBF \u0111\u1EC3 chuy\u1EC3n \u0111\u1ED5i c\u1EA5\
  u tr\xFAc d\u1EEF li\u1EC7u c\u1EE7a Go sang\u2026"
lastmod: '2024-03-13T22:44:36.011224-06:00'
model: gpt-4-0125-preview
summary: "Trong Go, g\xF3i `encoding/json` l\xE0 c\xE1nh c\u1ED5ng c\u1EE7a b\u1EA1\
  n \u0111\u1EBFn v\u1EDBi vi\u1EC7c \u0111i\u1EC1u khi\u1EC3n JSON, cung c\u1EA5\
  p c\u01A1 ch\u1EBF \u0111\u1EC3 chuy\u1EC3n \u0111\u1ED5i c\u1EA5u tr\xFAc d\u1EEF\
  \ li\u1EC7u c\u1EE7a Go sang JSON (m\xE3 h\xF3a) v\xE0 ng\u01B0\u1EE3c l\u1EA1i\
  \ (gi\u1EA3i m\xE3)."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Làm thế nào:
Trong Go, gói `encoding/json` là cánh cổng của bạn đến với việc điều khiển JSON, cung cấp cơ chế để chuyển đổi cấu trúc dữ liệu của Go sang JSON (mã hóa) và ngược lại (giải mã). Dưới đây là các ví dụ cơ bản để bạn bắt đầu:

### Mã hóa (Marshalling)
Để chuyển đổi một struct của Go sang JSON, bạn có thể sử dụng `json.Marshal`. Xem xét struct Go sau:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID        int      `json:"id"`
    Username  string   `json:"username"`
    Languages []string `json:"languages"`
}

func main() {
    user := User{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(userJSON))
}
```

Đầu ra:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### Giải mã (Unmarshalling)
Để phân tích JSON thành một cấu trúc dữ liệu của Go, sử dụng `json.Unmarshal`:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var user User
    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user)
}
```

Với struct `User` như trước, đoạn mã này phân tích chuỗi JSON thành một thể hiện của User.

Đầu ra:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## Sâu hơn
Gói `encoding/json` trong Go cung cấp một API trực quan, che giấu nhiều sự phức tạp liên quan đến việc điều khiển JSON. Được giới thiệu sớm trong quá trình phát triển của Go, gói này phản ánh triết lý đơn giản và hiệu quả của Go. Tuy nhiên, việc sử dụng phản xạ của `encoding/json` để kiểm tra và chỉnh sửa các struct tại thời gian chạy có thể dẫn đến hiệu suất kém hơn trong các kịch bản sử dụng nhiều CPU.

Những phương án thay thế như `json-iterator/go` và `ffjson` đã xuất hiện, cung cấp việc xử lý JSON nhanh hơn bằng cách tạo ra mã mã hóa và giải mã tĩnh. Tuy nhiên, `encoding/json` vẫn là gói được sử dụng phổ biến nhất do sự đơn giản, bền vững của nó và thực tế là nó là một phần của thư viện chuẩn, đảm bảo tính tương thích và ổn định qua các phiên bản Go.

Mặc dù hiệu suất tương đối chậm hơn, sự dễ sử dụng và sự tích hợp với hệ thống loại của Go làm cho `encoding/json` phù hợp với hầu hết các ứng dụng. Đối với những người làm việc trong các bối cảnh mà hiệu suất là tối quan trọng, việc khám phá các thư viện bên ngoài có thể đáng giá, nhưng đối với nhiều người, thư viện chuẩn cung cấp sự cân bằng đúng đắn giữa tốc độ, đơn giản và độ tin cậy.
