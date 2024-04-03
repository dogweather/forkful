---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:25.547212-07:00
description: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p ch\xE8n gi\xE1 tr\u1ECB v\xE0o m\u1ED9\
  t m\u1EABu chu\u1ED7i gi\u1ED1ng nh\u01B0 t\u1EEBng m\u1EA3nh gh\xE9p c\u1EE7a m\u1ED9\
  t c\xE2u \u0111\u1ED1, khi\u1EBFn chu\u1ED7i tr\u1EDF n\xEAn \u0111\u1ED9ng v\xE0\
  \ m\xE3 s\u1EA1ch h\u01A1n. L\u1EADp tr\xECnh vi\xEAn s\u1EED\u2026"
lastmod: '2024-03-13T22:44:36.914783-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p ch\xE8n gi\xE1 tr\u1ECB v\xE0o m\u1ED9\
  t m\u1EABu chu\u1ED7i gi\u1ED1ng nh\u01B0 t\u1EEBng m\u1EA3nh gh\xE9p c\u1EE7a m\u1ED9\
  t c\xE2u \u0111\u1ED1, khi\u1EBFn chu\u1ED7i tr\u1EDF n\xEAn \u0111\u1ED9ng v\xE0\
  \ m\xE3 s\u1EA1ch h\u01A1n."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

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
