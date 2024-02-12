---
title:                "Làm việc với TOML"
aliases:
- /vi/python/working-with-toml/
date:                  2024-01-28T22:11:33.053460-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
TOML, viết tắt của Tom's Obvious, Minimal Language, là một định dạng tuần tự hóa dữ liệu tương tự như JSON hoặc YAML, nhưng mục tiêu là sự đơn giản và dễ đọc. Lập trình viên sử dụng TOML cho các tệp cấu hình vì nó dễ viết và hiểu, và nó ánh xạ gọn gàng vào các cấu trúc dữ liệu trong các ngôn ngữ lập trình như Python.

## Làm thế nào:
Trước khi đi sâu, hãy cài đặt gói `toml` với `pip install toml`. Hãy phân tích một tệp TOML:

```python
import toml

# Ví dụ nội dung TOML dưới dạng chuỗi
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # Ngày đẳng cấp đầu tiên

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# Phân tích chuỗi TOML
parsed_toml = toml.loads(toml_string)

# Truy cập dữ liệu
print(parsed_toml['owner']['name'])  # Kết quả: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # Kết quả: [8001, 8001, 8002]
```

## Sâu hơn nữa
TOML được tạo ra bởi Tom Preston-Werner, một trong những người sáng lập GitHub, như một định dạng tệp cấu hình thân thiện với người dùng hơn. Nó được thiết kế để ánh xạ không mơ hồ vào một bảng băm và dễ dàng phân tích bởi máy móc.

So với JSON, TOML dễ đọc hơn cho các tệp cấu hình và hỗ trợ bình luận. YAML, một lựa chọn khác, có thể gọn gàng hơn, nhưng sự phụ thuộc vào thụt lề và các vấn đề tinh tế, như việc không cho phép sử dụng tab, có thể làm người ta vấp phải.

Về chi tiết thực hiện, giá trị TOML được kiểu hóa, bao gồm chuỗi, số nguyên, số thực, boolean, datetime, mảng và bảng. Mọi thứ đều phân biệt chữ hoa chữ thường. Ngoài ra, TOML hỗ trợ chuỗi nhiều dòng và, ở phiên bản mới nhất, thậm chí cho phép mảng với các kiểu khác nhau.

Python sử dụng thư viện `toml`, mà phản ánh thư viện JSON và YAML về mặt API. Bạn có `toml.load` và `toml.loads` để đọc TOML từ một tệp hoặc một chuỗi tương ứng, và `toml.dump` và `toml.dumps` để viết ra nó.

## Xem Thêm
- Kho chính thức TOML trên GitHub cho các đặc tả: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tài liệu thư viện Python `toml`: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Các ví dụ thực tế về TOML: Các tệp cấu hình cho trình quản lý gói của Rust `cargo` hoặc công cụ đóng gói Python `poetry`.
