---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:25.547212-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Trong PowerShell, b\u1EA1n n\u1ED9i suy b\u1EB1\
  ng c\xE1ch s\u1EED d\u1EE5ng chu\u1ED7i trong d\u1EA5u ngo\u1EB7c k\xE9p v\xE0 k\xFD\
  \ t\u1EF1 `$` tr\u01B0\u1EDBc t\xEAn bi\u1EBFn. \u0110\xF3ng g\xF3i bi\u1EC3u th\u1EE9\
  c trong `$()` \u0111\u1EC3 \u0111\xE1nh\u2026"
lastmod: '2024-03-13T22:44:36.914783-06:00'
model: gpt-4-0125-preview
summary: "Trong PowerShell, b\u1EA1n n\u1ED9i suy b\u1EB1ng c\xE1ch s\u1EED d\u1EE5\
  ng chu\u1ED7i trong d\u1EA5u ngo\u1EB7c k\xE9p v\xE0 k\xFD t\u1EF1 `$` tr\u01B0\u1EDB\
  c t\xEAn bi\u1EBFn."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

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
