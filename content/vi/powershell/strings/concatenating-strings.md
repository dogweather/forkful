---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:20.426371-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y \u0111i th\u1EB3ng v\xE0o v\u1EA5\
  n \u0111\u1EC1."
lastmod: '2024-03-13T22:44:36.922721-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y \u0111i th\u1EB3ng v\xE0o v\u1EA5n \u0111\u1EC1."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

## Cách thực hiện:
Hãy đi thẳng vào vấn đề:

```PowerShell
# Sử dụng toán tử '+'
$greeting = 'Hello, ' + 'World!'
$greeting # Xuất ra: Hello, World!

# Qua nội suy chuỗi
$name = 'Jane'
$welcomeMessage = "Hi, $name, nice to meet you!"
$welcomeMessage # Xuất ra: Hi, Jane, nice to meet you!

# Với toán tử -f (toán tử định dạng)
$city = 'New York'
$visitMessage = 'Welcome to {0}!' -f $city
$visitMessage # Xuất ra: Welcome to New York!

# StringBuilder cho các tình huống phức tạp (hơi quá mức cho những việc đơn giản)
$textBuilder = New-Object System.Text.StringBuilder
[void]$textBuilder.Append('PowerShell ')
[void]$textBuilder.Append('is ')
[void]$textBuilder.Append('awesome.')
$textBuilder.ToString() # Xuất ra: PowerShell is awesome.
```

## Đào sâu thêm
Trong quá khứ, việc nối chuỗi trong các ngôn ngữ lập trình trước đây khá là thô sơ - hãy nghĩ đến việc dùng băng dính để kết dính các câu lại với nhau. Trong PowerShell, đó là một việc dễ dàng.

Có nhiều cách để hoàn thành công việc. Toán tử '+' đơn giản nhưng có thể chậm với nhiều chuỗi. Nội suy chuỗi với "$variable" sạch sẽ hơn, và tuyệt vời cho việc chèn biến vào chuỗi. Toán tử định dạng '-f' tỏa sáng trong các tình huống mẫu.

Về hiệu suất - nếu bạn đang kết hợp một bài luận dài các chuỗi, bạn sẽ muốn một cái gì đó hợp lý hơn. Nhập `StringBuilder`. Nó không nối chuỗi ngay lập tức; thay vào đó, nó kết chuỗi của bạn lại với nhau khi được gọi, tiết kiệm thời gian và bộ nhớ cho những nhiệm vụ nối chuỗi lớn.

## Xem thêm
- [Về Join](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_join?view=powershell-7.3)
- [Về Biến Tự Động](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.3) (xem `$OFS`)
- Để biết thêm về định dạng chuỗi, hãy xem [Composite Formatting](https://docs.microsoft.com/en-us/dotnet/standard/base-types/composite-formatting).
- Và, nếu bạn có hứng thú, đây là thông tin chi tiết về [StringBuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-6.0).
