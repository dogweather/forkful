---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:54.750937-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Go, t\xE1i c\u1EA5u tr\xFAc c\xF3 th\u1EC3\
  \ dao \u0111\u1ED9ng t\u1EEB nh\u1EEFng ch\u1EC9nh s\u1EEDa m\xE3 \u0111\u01A1n\
  \ gi\u1EA3n \u0111\u1EBFn nh\u1EEFng thay \u0111\u1ED5i ph\u1EE9c t\u1EA1p h\u01A1\
  n. H\xE3y b\u1EAFt \u0111\u1EA7u v\u1EDBi m\u1ED9t v\xED d\u1EE5 c\u01A1 b\u1EA3\
  n: \u0111\u01A1n\u2026"
lastmod: '2024-03-13T22:44:35.993874-06:00'
model: gpt-4-0125-preview
summary: "Trong Go, t\xE1i c\u1EA5u tr\xFAc c\xF3 th\u1EC3 dao \u0111\u1ED9ng t\u1EEB\
  \ nh\u1EEFng ch\u1EC9nh s\u1EEDa m\xE3 \u0111\u01A1n gi\u1EA3n \u0111\u1EBFn nh\u1EEF\
  ng thay \u0111\u1ED5i ph\u1EE9c t\u1EA1p h\u01A1n."
title: "T\xE1i c\u1EA5u tr\xFAc"
weight: 19
---

## Làm thế nào:
Trong Go, tái cấu trúc có thể dao động từ những chỉnh sửa mã đơn giản đến những thay đổi phức tạp hơn. Hãy bắt đầu với một ví dụ cơ bản: đơn giản hóa một hàm Go ban đầu để cải thiện khả năng đọc và hiệu quả.

**Trước khi Tái Cấu Trúc:**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    var total float64
    if quantity > 0 {
        total = float64(quantity) * price
    } else {
        total = 0
    }
    return total
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // Kết quả: 59.9
}
```

**Sau khi Tái Cấu Trúc:**

```go
package main

import "fmt"

func CalculatePrice(quantity int, price float64) float64 {
    if quantity > 0 {
        return float64(quantity) * price
    }
    return 0
}

func main() {
    fmt.Println(CalculatePrice(10, 5.99))  // Kết quả: 59.9
}
```

Trong phiên bản đã tái cấu trúc, `else` được loại bỏ, làm đơn giản dòng chảy của hàm mà không ảnh hưởng đến kết quả đầu ra—một ví dụ về kỹ thuật tái cấu trúc cơ bản nhưng có ảnh hưởng trong Go.

Đối với một ví dụ nâng cao hơn, hãy xem xét tái cấu trúc các hàm để sử dụng giao diện cho khả năng tái sử dụng và kiểm thử tốt hơn:

**Trước khi Tái Cấu Trúc:**

```go
package main

import "fmt"

type Logger struct{}

func (l Logger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // Hãy tưởng tượng sự xử lý dữ liệu ở đây
    logger.Log("Dữ liệu đã được xử lý")
}

func main() {
    logger := Logger{}
    ProcessData("dữ liệu ví dụ", logger)
}
```

**Sau khi Tái Cấu Trúc:**

```go
package main

import "fmt"

type Logger interface {
    Log(message string)
}

type ConsoleLogger struct{}

func (c ConsoleLogger) Log(message string) {
    fmt.Println("Log:", message)
}

func ProcessData(data string, logger Logger) {
    // Xử lý dữ liệu vẫn không thay đổi
    logger.Log("Dữ liệu đã được xử lý")
}

func main() {
    logger := ConsoleLogger{}
    ProcessData("dữ liệu ví dụ", logger)
}
```

Việc tái cấu trúc sử dụng một giao diện (`Logger`) thay vì một kiểu cụ thể (`ConsoleLogger`) làm tăng tính linh hoạt của hàm và tách biệt quá trình xử lý dữ liệu khỏi cài đặt log cụ thể.

## Sâu Hơn Nữa
Tái cấu trúc trong Go phải cân bằng giữa sự đơn giản (một trong những triết lý cốt lõi của Go) với tính linh hoạt cần thiết trong các dự án phần mềm lớn. Với cách tiếp cận tối giản đối với các tính năng—không có generics (cho đến gần đây) và với sự nhấn mạnh mạnh mẽ vào khả năng đọc—ngôn ngữ tự nhiên hướng dẫn nhà phát triển về phía các cấu trúc mã dễ bảo trì hơn, đơn giản hơn. Tuy nhiên, điều này không có nghĩa là mã Go không hưởng lợi từ việc tái cấu trúc; nó chỉ có nghĩa là việc tái cấu trúc luôn cần ưu tiên sự rõ ràng và đơn giản.

Trong lịch sử, sự thiếu một số tính năng của Go (ví dụ, generics trước Go 1.18) đã dẫn đến các giải pháp sáng tạo nhưng đôi khi phức tạp cho việc tái sử dụng mã và tính linh hoạt, làm cho việc tái cấu trúc cho tính trừu tượng trở thành một thực hành phổ biến. Với việc giới thiệu generics trong Go 1.18, các nhà phát triển Go giờ đây đang tái cấu trúc mã kế thừa để tận dụng tính năng này cho sự an toàn kiểu dữ liệu và tái sử dụng mã tốt hơn, minh họa cho bản chất phát triển của các thực hành tái cấu trúc trong Go.

Tuy nhiên, bộ công cụ của Go, bao gồm `gofmt` cho việc định dạng mã và `go vet` cho việc xác định các cấu trúc đáng ngờ, hỗ trợ duy trì cơ sở mã sạch, giảm nhu cầu cho việc tái cấu trúc rộng rãi. Mặc dù tái cấu trúc là một công cụ vô giá trong kho vũ khí của một lập trình viên Go, việc sử dụng khôn ngoan các tính năng và công cụ của Go ngay từ đầu có thể giúp giảm thiểu nhu cầu cho việc tái cấu trúc phức tạp sau này.
