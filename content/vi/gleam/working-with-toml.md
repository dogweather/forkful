---
title:                "Làm việc với TOML"
date:                  2024-01-28T22:11:13.328785-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với TOML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Làm việc với TOML có nghĩa là phân tích cú pháp và tạo ra các tệp TOML (Tom's Obvious, Minimal Language - Ngôn ngữ Tối thiểu, Rõ ràng của Tom) bằng mã lệnh. Lập trình viên sử dụng TOML cho các tệp cấu hình dễ đọc và serial hóa dữ liệu, nhờ vào semantics rõ ràng và khả năng tương thích với các kiểu dữ liệu thông thường.

## Làm thế nào:
Gleam không có hỗ trợ TOML tích hợp sẵn, do đó bạn cần một thư viện bên ngoài. Ví dụ:

```gleam
// Giả sử bạn có một thư viện phân tích cú pháp TOML:
import toml/{Parser, Encoder}

// Phân tích cú pháp nội dung TOML
let toml_content = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// Sử dụng dữ liệu đã phân tích
match parsed {
  Ok(data) -> "Dữ liệu được phân tích thành công!"
  Error(_) -> "Không phân tích được dữ liệu."
}

// Tạo nội dung TOML từ cấu trúc dữ liệu Gleam
let data = #{
  "owner": #{
    "name": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

Output mẫu:

```
Dữ liệu được phân tích thành công!
```

## Tìm hiểu kỹ hơn
TOML được phát hành vào năm 2013 bởi Tom Preston-Werner. Mục tiêu: dễ đọc và đơn giản hơn XML và ít phức tạp hơn YAML cho cấu hình tệp. Bất chấp sự đơn giản, nó vẫn mạnh mẽ cho dữ liệu có cấu trúc, cung cấp cú pháp rõ ràng và dễ hiểu. Các lựa chọn thay thế bao gồm JSON, YAML và INI, nhưng cú pháp tối giản và rõ ràng của TOML thường chiến thắng cho các tệp cấu hình. Việc triển khai TOML trong Gleam bao gồm hai hành động chính: phân tích cú pháp TOML thành các cấu trúc dữ liệu gốc và serial hóa các cấu trúc dữ liệu gốc thành TOML. Hầu hết các thư viện TOML cho Erlang hoặc Elixir có thể được sử dụng trong Gleam do khả năng tương thích giữa các ngôn ngữ trên BEAM, đảm bảo tích hợp mượt mà trong các dự án Gleam.

## Xem thêm
- Đặc tả ngôn ngữ TOML: [https://toml.io/en/](https://toml.io/en/)
- Một bộ phân tích TOML của Erlang: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- TOML trên GitHub: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)
