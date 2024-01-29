---
title:                "Nội suy chuỗi ký tự"
date:                  2024-01-28T22:02:25.547212-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?
Nội suy chuỗi cho phép chèn giá trị vào một mẫu chuỗi giống như từng mảnh ghép của một câu đố, khiến chuỗi trở nên động và mã sạch hơn. Lập trình viên sử dụng nó để chèn biến, biểu thức, và định dạng trực tiếp trong chuỗi, giảm bớt lượng công việc nối chuỗi.

## Làm Thế Nào:
Trong PowerShell, bạn nội suy bằng cách sử dụng chuỗi trong dấu ngoặc kép và ký tự `$` trước tên biến. Đóng gói biểu thức trong `$()` để đánh giá chúng ngay bên trong chuỗi.

```PowerShell
$name = "Alex"
$day = (Get-Date).DayOfWeek

# Nội suy biến cơ bản
"Xin chào, $name! Chúc bạn một ngày $day vui vẻ!"

# Nội suy biểu thức
"Pi làm tròn đến hai chữ số thập phân là $(Math::Round([Math]::Pi, 2))"

# Đầu ra
Xin chào, Alex! Chúc bạn một ngày Wednesday vui vẻ!
Pi làm tròn đến hai chữ số thập phân là 3.14
```

## Sâu hơn nữa
PowerShell đã áp dụng nội suy chuỗi từ những ngôn ngữ lập trình trước đó như Perl. Trước PowerShell v3, chúng ta đã nối chuỗi bằng toán tử `+` hoặc sử dụng toán tử định dạng `-f`. Dưới đây là sự tiến hóa:

- Nối chuỗi kiểu cũ: `"Xin chào, " + $name + "! Hôm nay là " + $day + "."`
- Toán tử định dạng: `"Xin chào, {0}! Hôm nay là {1}." -f $name, $day`

Chuỗi được nội suy dễ đọc và ít lỗi hơn. Đằng sau hậu trường, PowerShell giải thích chuỗi nội suy và thay thế các biến hoặc biểu thức bằng giá trị của chúng khi chuỗi được đánh giá, không phải khi nó được định nghĩa.

## Xem Thêm
- [Giải thích toán tử định dạng](https://ss64.com/ps/syntax-f-operator.html)
