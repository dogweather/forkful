---
title:                "Làm việc với JSON"
aliases: - /vi/powershell/working-with-json.md
date:                  2024-01-28T22:11:03.686181-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
JSON (JavaScript Object Notation) là một định dạng dữ liệu nhẹ, dễ đọc và viết cho con người, dễ phân tích và tạo ra cho máy móc. Lập trình viên làm việc với JSON để trao đổi dữ liệu giữa các máy khách và máy chủ web hoặc để lưu trữ dữ liệu bởi vì nó đơn giản và đã trở thành một chuẩn web.

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
