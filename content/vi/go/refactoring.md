---
title:                "Tái cấu trúc mã"
date:                  2024-01-28T22:06:29.877387-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tái cấu trúc mã"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tái cấu trúc là quá trình cải tổ lại mã máy tính hiện có mà không thay đổi hành vi bên ngoài của nó. Lập trình viên thực hiện điều này để cải thiện các thuộc tính không chức năng của phần mềm, như khả năng đọc và bảo trì, có thể làm cho mã dễ hiểu hơn, giảm độ phức tạp và giúp dễ dàng phát hiện lỗi hơn.

## Làm thế nào:
Hãy cùng khám phá một ví dụ tái cấu trúc code Go đơn giản. Chúng ta sẽ lấy một đoạn mã tính trung bình của một dãy số và tái cấu trúc nó để tăng tính rõ ràng và khả năng tái sử dụng.

Mã gốc:
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Average:", average)
}
```

Mã đã tái cấu trúc:
```Go
package main

import "fmt"

// CalculateAverage nhận vào một slice của float64 và trả về giá trị trung bình.
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Average:", average)
}
```

Trong mã đã tái cấu trúc, chúng ta đã tách logic tính trung bình vào một hàm riêng biệt có tên là `CalculateAverage`. Điều này làm cho hàm `main` ngắn gọn hơn và logic tính toán giá trị trung bình có thể tái sử dụng và kiểm thử.

## Đi sâu hơn
Tái cấu trúc mã không phải là một khái niệm hiện đại; nó xuất hiện trước khi máy tính được sử dụng rộng rãi. Quá trình này có thể bắt đầu từ lĩnh vực kỹ thuật cơ khí hoặc thậm chí sớm hơn. Trong lĩnh vực phần mềm, nó trở nên được hình thức hóa hơn với sự xuất hiện của lập trình hướng đối tượng và lập trình cực đoan (XP) vào những năm 1990, đặc biệt chịu ảnh hưởng từ cuốn sách điển hình của Martin Fowler "Refactoring: Improving the Design of Existing Code."

Có nhiều kỹ thuật tái cấu trúc, từ đổi tên biến cho dễ hiểu đến các mẫu phức tạp hơn như trích xuất phương thức hoặc lớp. Điều quan trọng là thực hiện những thay đổi nhỏ, dần dần không làm thay đổi chức năng của phần mềm nhưng cải thiện cấu trúc nội bộ.

Khi sử dụng Go, việc tái cấu trúc có thể khá đơn giản do sự đơn giản và thư viện chuẩn mạnh mẽ của ngôn ngữ. Tuy nhiên, vẫn quan trọng phải có một bộ kiểm thử đơn vị tốt để đảm bảo rằng tái cấu trúc không giới thiệu lỗi. Công cụ như `gorename` và `gofmt` giúp tự động hóa một số quá trình, và các môi trường phát triển tích hợp (IDE) thường có hỗ trợ tái cấu trúc tích hợp.

Ngoài tái cấu trúc thủ công, còn có một số công cụ tái cấu trúc mã tự động available cho Go, như công cụ tái cấu trúc của GoLand và Go Refactor. Mặc dù những công cụ này có thể tăng tốc quá trình, chúng không phải là phương tiện thay thế cho việc hiểu biết mã và thực hiện các thay đổi được suy nghĩ.

## Xem thêm
 - [Refactoring trong Go: Đơn giản là đẹp](https://go.dev/blog/slices)
 - [Go hiệu quả: Tái cấu trúc với Interfaces](https://go.dev/doc/effective_go#interfaces)
 - [Trang Refactoring của Martin Fowler](https://refactoring.com/)
 - [Công cụ tái cấu trúc của GoLand](https://www.jetbrains.com/go/features/refactorings/)
