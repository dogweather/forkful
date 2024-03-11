---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:40.622214-07:00
description: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng trao \u0111\u1ED5i d\u1EEF li\u1EC7u ph\u1ED5 bi\u1EBFn tr\xEAn web. L\u1EAD\
  p tr\xECnh vi\xEAn s\u1EED d\u1EE5ng JSON \u0111\u1EC3 d\u1EC5 d\xE0ng truy\u1EC1\
  n d\u1EEF li\u1EC7u gi\u1EEFa m\xE1y ch\u1EE7\u2026"
lastmod: '2024-03-11T00:14:09.369708-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng trao \u0111\u1ED5i d\u1EEF li\u1EC7u ph\u1ED5 bi\u1EBFn tr\xEAn web. L\u1EAD\
  p tr\xECnh vi\xEAn s\u1EED d\u1EE5ng JSON \u0111\u1EC3 d\u1EC5 d\xE0ng truy\u1EC1\
  n d\u1EEF li\u1EC7u gi\u1EEFa m\xE1y ch\u1EE7\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
---

{{< edit_this_page >}}

## Gì và Tại sao?
JSON (JavaScript Object Notation) là một định dạng trao đổi dữ liệu phổ biến trên web. Lập trình viên sử dụng JSON để dễ dàng truyền dữ liệu giữa máy chủ và các máy khách web do tính đơn giản của nó và thực tế là nó được hiểu một cách tự nhiên bởi JavaScript, và do đó, bởi trình duyệt web.

## Làm thế nào:
Làm việc với JSON trong Python yêu cầu mô-đun `json`. Dưới đây là một hướng dẫn nhanh:

### Phân tích cú pháp JSON (`json.loads`):
```Python
import json

# Hãy tưởng tượng bạn nhận được JSON từ một API
json_string = '{"name": "Alice", "age": 30, "city": "Wonderland"}'

# Phân tích cú pháp chuỗi JSON thành một từ điển Python
person = json.loads(json_string)

print(person)
```

### Đầu ra Mẫu:
```Python
{'name': 'Alice', 'age': 30, 'city': 'Wonderland'}
```

### Tạo JSON (`json.dumps`):
```Python
import json

# Từ điển Python
person_dict = {'name': 'Alice', 'age': 30, 'city': 'Wonderland'}

# Chuyển đổi từ điển thành một chuỗi được định dạng JSON
person_json = json.dumps(person_dict)

print(person_json)
```

### Đầu ra Mẫu:
```Python
'{"name": "Alice", "age": 30, "city": "Wonderland"}'
```

## Sâu hơn
JSON được đề xuất bởi Douglas Crockford vào đầu những năm 2000 như một phần của ngôn ngữ JavaScript, nhưng nhanh chóng được chấp nhận trên các ngôn ngữ khác do định dạng nhẹ của nó. Các lựa chọn thay thế cho JSON bao gồm XML và YAML, nhưng JSON chiến thắng vì sự tối giản và tốc độ. Trực tiếp trong Python, JSON được chuyển thành chuỗi và được giải mã thành từ điển hoặc danh sách, làm cho việc làm việc với nó trở nên dễ dàng theo cách lập trình. Lưu ý rằng mặc dù JSON giống như một từ điển Python, chúng không giống nhau - bạn không thể sử dụng các đối tượng và loại cụ thể của Python trong JSON.

## Xem thêm
- Trang web chính thức của JSON: [json.org](https://www.json.org)
- Tài liệu mô-đun JSON của Python: [Python JSON](https://docs.python.org/3/library/json.html)
- So sánh giữa JSON và XML: [JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)
- Tài liệu Python 3.x: [python.org](https://www.python.org/doc/)
