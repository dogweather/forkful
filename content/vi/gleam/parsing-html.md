---
title:                "Phân Tích Cú Pháp HTML"
date:                  2024-01-28T22:04:12.720365-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân Tích Cú Pháp HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Phân tích cú pháp HTML có nghĩa là biến các chuỗi HTML thành dữ liệu có cấu trúc. Các lập trình viên làm điều này để thao tác hoặc trích xuất thông tin từ các trang web hoặc để an toàn tạo HTML từ đầu vào của người dùng.

## Làm thế nào:
Gleam không có thư viện phân tích cú pháp HTML sẵn có, nhưng bạn có thể sử dụng các thư viện Erlang thông qua giao tiếp. Dưới đây là một ví dụ cơ bản sử dụng gói `meeseeks`, một phân tích cú pháp HTML/XML:

Đầu tiên, thêm `meeseeks` vào phụ thuộc `rebar.config` của bạn, như sau:

```erlang
{deps, [
    {meeseeks, "0.15.0"}
]}.
```

Dưới đây là cách bạn có thể phân tích và tìm kiếm HTML trong Gleam, giả định rằng bạn đã xử lý giao tiếp Erlang một cách chính xác:

```gleam
import gleam/erlang
import meeseeks/html
import meeseeks/css

pub fn parse_and_find() -> Result(String, Nil) {
  let html = "<html><body><h1>Xin chào, Gleam!</h1></body></html>"
  let doc = html |> html.parse
  let selector = css.parse("h1").unwrap()
  
  doc
  |> meeseeks.all(selector)
  |> meeseeks.text
  |> Result.map(lists.head)
}
```
Hàm này phân tích cú pháp HTML, sau đó truy vấn nó cho các thẻ `h1` và lấy văn bản. Dưới đây là những gì chạy nó có thể xuất:

```shell
> parse_and_find()
Ok("Xin chào, Gleam!")
```

## Sâu hơn
Trong quá khứ, phân tích cú pháp HTML trong một ngôn ngữ mới có nghĩa là viết một phân tích cú pháp tùy chỉnh hoặc bọc một cái hiện có. Các lựa chọn khác bao gồm sử dụng regex (thường là một ý tưởng xấu do độ phức tạp của HTML) hoặc các thư viện vững chắc như `meeseeks` dựa trên các phân tích cú pháp đã được chứng minh (như `html5ever` từ Rust trong trường hợp của `meeseeks`).

Thực hiện phân tích cú pháp HTML có thể trở nên phức tạp bởi vì HTML thường không được hình thành tốt hoặc dự đoán được. Các thư viện xử lý điều này bằng cách làm sạch và chuẩn hóa dữ liệu. Giao tiếp với các thư viện Erlang từ Gleam đơn giản nhờ vào sự tương thích của hệ sinh thái Erlang, cung cấp quyền truy cập vào các thư viện đã trưởng thành mà không cần phải tái tạo lại bánh xe.

## Xem thêm
Để biết thêm đọc và nguồn lực, hãy xem:

- Thư viện Meeseeks trên Hex: https://hex.pm/packages/meeseeks
- Bộ phân tích cú pháp Rust `html5ever`: https://github.com/servo/html5ever
- Hướng dẫn tương tác Erlang cho Gleam: https://gleam.run/book/tour/erlang-interop/
