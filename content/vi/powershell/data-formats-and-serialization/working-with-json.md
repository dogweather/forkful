---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:03.686181-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: ."
lastmod: '2024-03-13T22:44:36.964043-06:00'
model: gpt-4-0125-preview
summary: ''
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

## Cách thực hiện:


### Đọc JSON
```PowerShell
# Giả sử 'data.json' chứa {"name": "John", "age": 30}
$json = Get-Content -Path 'data.json' | ConvertFrom-Json
# Xuất tên
$json.name  # Kết quả: John
```

### Viết JSON
```PowerShell
$person = @{name='Jane'; age=25}
$person | ConvertTo-Json | Set-Content -Path 'person.json'
# person.json giờ đây chứa: 
# {
#     "age":  25,
#     "name":  "Jane"
# }
```

### Chỉnh sửa JSON
```PowerShell
$json = Get-Content -Path 'person.json' | ConvertFrom-Json
$json.age = 26
$json | ConvertTo-Json | Set-Content -Path 'person.json'
# person.json giờ đây cập nhật tuổi của Jane thành 26
```

## Sâu hơn
JSON đã trở thành lựa chọn hàng đầu cho dữ liệu web kể từ đầu những năm 2000, giành lấy ngôi vương từ XML nhờ sự đơn giản của nó. Các phương án thay thế cho JSON bao gồm YAML và TOML mới hơn, nhưng JSON vẫn thống trị do sự hỗ trợ rộng rãi và sự phù hợp với cú pháp đối tượng của JavaScript. Khi làm việc với JSON trong PowerShell, các cmdlet `ConvertFrom-Json` và `ConvertTo-Json` tích hợp sẵn rất mạnh mẽ, nhưng hãy chú ý đến giới hạn độ sâu của chúng và loại `[PSCustomObject]` PowerShell được sử dụng khi chuyển đổi từ JSON.

## Xem thêm
- [JSON.org](https://www.json.org/json-en.html) cho cú pháp và cơ bản của JSON
- [MDN Web Docs về JSON](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON) về phần JavaScript
