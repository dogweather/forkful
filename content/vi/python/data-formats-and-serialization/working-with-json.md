---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:40.622214-07:00
description: "L\xE0m th\u1EBF n\xE0o: L\xE0m vi\u1EC7c v\u1EDBi JSON trong Python\
  \ y\xEAu c\u1EA7u m\xF4-\u0111un `json`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t h\u01B0\u1EDBng d\u1EABn nhanh."
lastmod: '2024-03-13T22:44:36.125392-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi JSON trong Python y\xEAu c\u1EA7u m\xF4-\u0111\
  un `json`."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

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

### Tạo JSON (`json.dumps`):
```Python
import json

# Từ điển Python
person_dict = {'name': 'Alice', 'age': 30, 'city': 'Wonderland'}

# Chuyển đổi từ điển thành một chuỗi được định dạng JSON
person_json = json.dumps(person_dict)

print(person_json)
```

## Sâu hơn
JSON được đề xuất bởi Douglas Crockford vào đầu những năm 2000 như một phần của ngôn ngữ JavaScript, nhưng nhanh chóng được chấp nhận trên các ngôn ngữ khác do định dạng nhẹ của nó. Các lựa chọn thay thế cho JSON bao gồm XML và YAML, nhưng JSON chiến thắng vì sự tối giản và tốc độ. Trực tiếp trong Python, JSON được chuyển thành chuỗi và được giải mã thành từ điển hoặc danh sách, làm cho việc làm việc với nó trở nên dễ dàng theo cách lập trình. Lưu ý rằng mặc dù JSON giống như một từ điển Python, chúng không giống nhau - bạn không thể sử dụng các đối tượng và loại cụ thể của Python trong JSON.

## Xem thêm
- Trang web chính thức của JSON: [json.org](https://www.json.org)
- Tài liệu mô-đun JSON của Python: [Python JSON](https://docs.python.org/3/library/json.html)
- So sánh giữa JSON và XML: [JSON vs XML](https://www.w3schools.com/js/js_json_xml.asp)
- Tài liệu Python 3.x: [python.org](https://www.python.org/doc/)
