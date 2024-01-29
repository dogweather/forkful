---
title:                "Xóa các ký tự phù hợp với một mẫu"
date:                  2024-01-28T21:59:16.324643-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Xóa ký tự khớp với mẫu là việc tìm các chuỗi cụ thể trong văn bản và loại bỏ chúng. Các lập trình viên làm điều này để làm sạch dữ liệu, phân tích thông tin cần thiết, hoặc làm sạch đầu vào.

## Làm Thế Nào:
Trong Gleam, bạn thường làm việc với mô-đun `String` cho việc thao tác với văn bản. Regex không được tích hợp sẵn, nhưng bạn có thể loại bỏ các mẫu cố định hoặc sử dụng các thư viện bên ngoài cho các nhiệm vụ phức tạp hơn. Hãy làm sạch một số văn bản bằng cách loại bỏ dấu chấm than từ một chuỗi.

```gleam
import gleam/string

pub fn remove_exclamations(text: String) -> String {
  string.replace(text, "!", "")
}

// Cách sử dụng
fn main() {
  let cleaned_text = remove_exclamations("Hello, World!!")
  assert cleaned_text == "Hello, World"
}
```
Đoạn mã này thay thế tất cả dấu chấm than bằng một chuỗi trống, loại bỏ chúng một cách hiệu quả.

## Tìm Hiểu Sâu
Gleam là một ngôn ngữ có kiểu dữ liệu tĩnh cho máy ảo Erlang, ưu tiên hiệu suất và độ tin cậy. Mô-đun String của nó cung cấp các chức năng thao tác cơ bản nhưng không có chức năng so khớp mẫu nâng cao tìm thấy trong các thư viện regex.

Về bối cảnh lịch sử, regex đã xuất hiện từ những năm 1950, bắt nguồn từ lý thuyết ngôn ngữ hình thức và lý thuyết automata. Hầu hết các ngôn ngữ lập trình đã áp dụng một số hình thức triển khai regex để so khớp mẫu.

Trong Gleam, để xử lý việc xóa mẫu phức tạp hơn, bạn sẽ thường tìm đến một thư viện Erlang hoặc một mô-đun Elixir thông qua giao tiếp vì hệ sinh thái của Gleam vẫn còn mới. Việc triển khai dựa vào sự ổn định của BEAM (máy ảo Erlang) và các thư viện lâu đời của nó.

Các phương án khác trong Gleam có thể bao gồm việc viết các hàm so khớp mẫu của riêng bạn cho các mẫu dễ dự đoán hơn hoặc xử lý các trường hợp cụ thể bằng các chức năng `String` như `slice`, `split`, hoặc `trim`.

## Xem Thêm
- Giới thiệu về Regex nói chung (không riêng Gleam): [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
