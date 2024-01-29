---
title:                "Viết hoa một chuỗi"
date:                  2024-01-28T21:55:36.807035-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết hoa một chuỗi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Ý nghĩa và Lý do

Việc viết hoa một chuỗi nghĩa là chuyển ký tự đầu tiên sang chữ hoa trong khi phần còn lại ở dạng chữ thường. Lập trình viên thực hiện điều này để chuẩn hóa dữ liệu nhập vào, vì mục đích phong cách hoặc tuân theo các quy tắc ngữ pháp trong giao diện người dùng.

## Cách thực hiện:

Trong Gleam, chúng ta có thể định nghĩa một hàm để viết hoa một chuỗi. Hiện tại, thư viện chuẩn của Gleam không trực tiếp cung cấp một hàm viết hoa, vì vậy chúng ta tự tạo một hàm sử dụng cách cắt chuỗi:

```gleam
import gleam/string

pub fn capitalize(text: String) -> String {
  let head = string.slice(text, 0, 1)
  let tail = string.slice(text, 1, string.len(text))
  
  string.append(string.uppercase(head), string.lowercase(tail))
}

pub fn main() {
  assert capitalize("hello") == "Hello"
  assert capitalize("WORLD") == "World"
}
```

Kết quả mẫu:

```plaintext
"Hello"
"World"
```

## Khám phá sâu hơn

Trong lịch sử, hàm viết hoa chuỗi thường được bao gồm trong thư viện chuẩn của nhiều ngôn ngữ. Tuy nhiên, Gleam, là một ngôn ngữ mới, có thể thiếu một số hàm tiện ích, để lại cho nhà phát triển việc tự thực hiện chúng. Các phương pháp thay thế khác để viết hoa chuỗi có thể bao gồm sử dụng biểu thức chính quy hoặc thư viện unicode cho những trường hợp phức tạp hơn xét đến các quy tắc cụ thể của ngôn ngữ. Trong ví dụ cơ bản của chúng tôi, chi tiết triển khai là đơn giản: chúng tôi chia chuỗi thành hai phần (đầu và đuôi), viết hoa ký tự đầu tiên và tái kết hợp chúng.

## Xem thêm

- Phân đoạn văn bản unicode cho các quy tắc phức tạp hơn: [https://unicode.org/reports/tr29/](https://unicode.org/reports/tr29/)
