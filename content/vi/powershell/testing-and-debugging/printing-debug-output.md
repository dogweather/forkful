---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:45.199938-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y gi\u1EEF n\xF3 \u0111\u01A1n gi\u1EA3\
  n v\xE0 th\u1EF1c s\u1EF1 l\xE0m \u0111i\u1EC1u g\xEC \u0111\xF3. Ch\xFAng ta s\u1EBD\
  \ hi\u1EC3n th\u1ECB gi\xE1 tr\u1ECB c\u1EE7a m\u1ED9t bi\u1EBFn, c\xE1ch m\u1ED9\
  t v\xF2ng l\u1EB7p ti\u1EBFn tri\u1EC3n, v\xE0 b\u1EAFt l\u1ED7i c\u1EE9ng \u0111\
  \u1EA7u\u2026"
lastmod: '2024-03-13T22:44:36.939907-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y gi\u1EEF n\xF3 \u0111\u01A1n gi\u1EA3n v\xE0 th\u1EF1c s\u1EF1 l\xE0\
  m \u0111i\u1EC1u g\xEC \u0111\xF3."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Làm thế nào:
Hãy giữ nó đơn giản và thực sự làm điều gì đó. Chúng ta sẽ hiển thị giá trị của một biến, cách một vòng lặp tiến triển, và bắt lỗi cứng đầu có thể xuất hiện.

```PowerShell
# Hiển thị giá trị của một biến
$name = "PowerShell Guru"
Write-Host "Giá trị của name là: $name"

# Giám sát tiến độ của một vòng lặp
for ($i = 0; $i -lt 5; $i++) {
    Write-Host "Chúng ta đang ở vòng lặp số: $i"
}

# Bắt và in một lỗi
try {
    Get-Item "C:\NonExistent\File.txt" -ErrorAction Stop
} catch {
    Write-Host "Ối: $_"
}
```

Đầu ra mẫu:

```
Giá trị của name là: PowerShell Guru
Chúng ta đang ở vòng lặp số: 0
Chúng ta đang ở vòng lặp số: 1
Chúng ta đang ở vòng lặp số: 2
Chúng ta đang ở vòng lặp số: 3
Chúng ta đang ở vòng lặp số: 4
Ối: Không thể tìm thấy đường dẫn 'C:\NonExistent\File.txt' vì nó không tồn tại.
```

## Sâu hơn
Trở lại những ngày xưa cũ của lập trình, sửa lỗi thường có nghĩa là lỗi vật lý thực sự gây rối với phần cứng. Chúng ta đã đi được một chặng đường dài kể từ đó, giờ đây sử dụng thuật ngữ "bug" cho các vấn đề về mã lệnh, và "debugging" để sửa chúng.

Lệnh `Write-Host` là người bạn PowerShell dùng để in ra màn hình, điều này tốt cho các kịch bản cơ bản. Nhưng có những cách mát mẻ hơn để làm điều đó: `Write-Verbose`, `Write-Debug`, `Write-Output`, và `Write-Information` giống như các hương vị đầu ra khác nhau cho các trường hợp sử dụng khác nhau. Chúng cho phép bạn kiểm soát độ chi tiết, điều này tuyệt vời khi bạn cần làm cho kịch bản của mình im lặng hoặc ghi chép mọi thứ mà không làm loãng bảng điều khiển.

Khi đến với thực thi, xử lý lỗi của PowerShell đặc biệt là sang trọng. Bạn có thể bắt các loại ngoại lệ khác nhau với các khối `try`, `catch`, `finally` và quyết định cách phản ứng. Đó giống như một cuộc phiêu lưu tự chọn cho quản lý lỗi.

## Xem thêm
- [Về Try, Catch, Finally](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-exceptions?view=powershell-7.1)
