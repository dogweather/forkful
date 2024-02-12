---
title:                "Làm việc với TOML"
aliases: - /vi/lua/working-with-toml.md
date:                  2024-01-28T22:11:27.473273-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Làm Thế Nào và Tại Sao?
Làm việc với TOML bao gồm việc phân tích cú pháp và tạo dữ liệu TOML (Tom's Obvious, Minimal Language) bằng Lua. Lập trình viên sử dụng TOML cho các tệp cấu hình do tính dễ đọc và cú pháp đơn giản của nó, dễ dàng chuyển đổi thành cấu trúc dữ liệu.

## Làm cách nào:
Đầu tiên, đảm bảo môi trường Lua của bạn có trình phân tích cú pháp TOML. Trong ví dụ này, chúng ta sẽ sử dụng `lua-toml`.

```Lua
local toml = require("toml")

-- Phân tích chuỗi TOML
local toml_data = [[
title = "Ví dụ TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "Ví dụ TOML"

-- Tạo chuỗi TOML
local table_data = {
  title = "Ví dụ TOML",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

Kết quả mẫu:
```
Ví dụ TOML
```

## Đi Sâu Hơn
TOML được tạo ra bởi Tom Preston-Werner vào năm 2013 như một lựa chọn thay thế cho các ngôn ngữ tuần tự hóa dữ liệu khác như XML và YAML, cung cấp một định dạng đơn giản hơn để biểu diễn dữ liệu cấu hình. Mặc dù JSON là phổ biến, nhưng cú pháp của nó có thể gây khó khăn cho các tệp cấu hình. TOML nổi bật với cú pháp rõ ràng hơn cho con người, giống với các tệp .ini nhưng với khả năng lồng nhau và các kiểu dữ liệu.

Các lựa chọn thay thế cho TOML bao gồm JSON, YAML và XML. Tuy nhiên, TOML được thiết kế cụ thể cho cấu hình và có thể nói là đơn giản hơn YAML, dễ đọc hơn JSON cho mục đích cấu hình và ít dài dòng hơn XML.

Việc thực hiện xử lý TOML trong Lua nói chung yêu cầu một thư viện bên thứ ba. Hiệu suất và các tính năng có thể khác nhau, từ việc phân tích cơ bản đến hỗ trợ tuần tự hóa đầy đủ. Khi xử lý các tệp cấu hình lớn hoặc thao tác đọc/ghi thường xuyên, cần xem xét hiệu suất của thư viện và sự tuân thủ với phiên bản TOML mới nhất.

## Xem Thêm
- Thông số kỹ thuật TOML: https://toml.io/en/
- Thư viện `lua-toml`: https://github.com/jonstoler/lua-toml
- So sánh các định dạng tuần tự hóa dữ liệu: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
