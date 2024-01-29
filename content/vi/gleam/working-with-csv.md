---
title:                "Làm việc với CSV"
date:                  2024-01-28T22:10:14.615386-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với các tệp CSV (Comma-Separated Values) có nghĩa là xử lý dữ liệu trong một định dạng dựa trên văn bản đơn giản, lưu trữ dữ liệu dạng bảng. Các lập trình viên sử dụng nó vì nó được hỗ trợ rộng rãi, dễ đọc và dễ phân tích cú pháp hoặc tạo ra.

## Làm thế nào:

Hiện tại, Gleam không có thư viện tiêu chuẩn chuyên dụng nào cho việc thao tác CSV, nhưng bạn có thể thực hiện việc phân tích cú pháp cơ bản với các hàm có sẵn. Dưới đây là một ví dụ đơn giản:

```gleam
import gleam/string

fn parse_csv_line(line: String) -> List(String) {
  string.split(line, ",")
}

pub fn main() {
  let csv_content = "name,age,city\nAlice,30,New York\nBob,22,Los Angeles"
  let lines = string.split(csv_content, "\n")
  case lines {
    [] -> []
    [.., _header | rows] -> rows
      |> list.map(parse_csv_line)
      |> io.debug
  }
}

// Mẫu đầu ra bên trong hàm `main`:
// [
//   ["Alice", "30", "New York"],
//   ["Bob", "22", "Los Angeles"],
// ]
```
Hãy nhớ xử lý các trường hợp ngoại lệ như dấu phẩy trong giá trị, dấu xuống dòng, và bộ nhận dạng văn bản trong một triển khai đầy đủ.

## Sâu hơn

CSV là một định dạng cũ, có từ những ngày đầu của máy tính, điều này góp phần vào việc nó được chấp nhận rộng rãi. Các lựa chọn thay thế như JSON hay XML cung cấp nhiều cấu trúc hơn nhưng có thể phức tạp hơn để phân tích cú pháp. Cách bạn xử lý dữ liệu CSV trong Gleam có thể liên quan đến việc sử dụng các thư viện bên ngoài nếu có, hoặc tạo một bộ phân tích cú pháp tùy chỉnh. Khi chuyển đổi sang CSV có thể yêu cầu thêm dấu phẩy và xuống dòng một cách cẩn thận, thoát khỏi các ký tự cần thiết.

## Xem thêm

- Để hiểu về các định dạng tệp khác nhau, hãy kiểm tra các thông số kỹ thuật [JSON](https://www.json.org/json-en.html) và [XML](https://www.w3.org/XML/).
- Đối với việc xử lý CSV phức tạp, hãy xem xét đóng góp hoặc sử dụng một thư viện CSV trong [hệ sinh thái Gleam](https://hex.pm/) khi có sẵn.
