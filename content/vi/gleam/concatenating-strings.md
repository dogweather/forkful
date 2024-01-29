---
title:                "Nối chuỗi ký tự"
date:                  2024-01-28T21:57:13.062748-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nối chuỗi ký tự"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Concatenating strings with Gleam

## Cái gì & Tại sao?

Nối chuỗi là kỹ thuật ghép hai hoặc nhiều chuỗi lại với nhau từ đầu đến cuối để tạo thành một chuỗi mới. Lập trình viên thực hiện việc này để xây dựng câu, kết hợp dữ liệu động với văn bản, hoặc tạo ra các mẫu mã cho sự tinh tế trong mã hóa.

## Làm thế nào:

Đi thẳng vào mã, đây là cách bạn "nhảy múa" với các chuỗi trong Gleam:

```gleam
fn main() {
  let greeting = "Hello"
  let subject = "World"
  let exclamation = "!"

  let message = greeting ++ " " ++ subject ++ exclamation
  message
}

// Kết quả mong đợi: "Hello World!"
```

Dễ như ăn bánh, phải không? Chỉ cần ghép các chuỗi lại với nhau bằng `++` và bạn sẽ có được một "nồi hầm" chuỗi.

## Sâu hơn nữa

Việc nối chuỗi có vẻ đơn giản, nhưng có nhiều điều phức tạp bên dưới. Trong lịch sử, nối chuỗi trong các ngôn ngữ lập trình có thể trở nên phức tạp với các loại khác nhau hoặc vấn đề về không thay đổi. Các phương án thay thế bao gồm định dạng chuỗi hoặc xây dựng với danh sách, nhưng nối chuỗi vẫn là lựa chọn phổ biến do sự đơn giản của nó.

Trong Gleam, nơi mà sự tinh khiết và kiểu dữ liệu mạnh được đề cao, nối chuỗi sử dụng toán tử `++` đảm bảo loại dữ liệu chính xác và kết quả là một chuỗi mới mỗi lần—không có tác động phụ ở đây.

## Xem thêm

Để biết thêm về các trò chơi dựa trên chuỗi:

- Giới thiệu về Gleam: [https://gleam.run](https://gleam.run)
