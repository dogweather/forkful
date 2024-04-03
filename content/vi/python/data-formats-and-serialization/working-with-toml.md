---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:33.053460-07:00
description: "L\xE0m th\u1EBF n\xE0o: Tr\u01B0\u1EDBc khi \u0111i s\xE2u, h\xE3y c\xE0\
  i \u0111\u1EB7t g\xF3i `toml` v\u1EDBi `pip install toml`. H\xE3y ph\xE2n t\xED\
  ch m\u1ED9t t\u1EC7p TOML."
lastmod: '2024-03-13T22:44:36.127894-06:00'
model: gpt-4-0125-preview
summary: "Tr\u01B0\u1EDBc khi \u0111i s\xE2u, h\xE3y c\xE0i \u0111\u1EB7t g\xF3i `toml`\
  \ v\u1EDBi `pip install toml`."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

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
