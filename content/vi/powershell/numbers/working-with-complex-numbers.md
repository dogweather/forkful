---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:31.931231-07:00
description: "S\u1ED1 ph\u1EE9c, nh\u1EEFng s\u1ED1 c\xF3 m\u1ED9t ph\u1EA7n th\u1EF1\
  c v\xE0 m\u1ED9t ph\u1EA7n \u1EA3o (nh\u01B0 3 + 4i), r\u1EA5t quan tr\u1ECDng trong\
  \ c\xE1c l\u0129nh v\u1EF1c nh\u01B0 k\u1EF9 thu\u1EADt, v\u1EADt l\xFD, v\xE0 khoa\
  \ h\u1ECDc d\u1EEF li\u1EC7u. C\xE1c l\u1EADp tr\xECnh\u2026"
lastmod: '2024-03-13T22:44:36.925330-06:00'
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c, nh\u1EEFng s\u1ED1 c\xF3 m\u1ED9t ph\u1EA7n th\u1EF1\
  c v\xE0 m\u1ED9t ph\u1EA7n \u1EA3o (nh\u01B0 3 + 4i), r\u1EA5t quan tr\u1ECDng trong\
  \ c\xE1c l\u0129nh v\u1EF1c nh\u01B0 k\u1EF9 thu\u1EADt, v\u1EADt l\xFD, v\xE0 khoa\
  \ h\u1ECDc d\u1EEF li\u1EC7u. C\xE1c l\u1EADp tr\xECnh\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
---

{{< edit_this_page >}}

## Điều gì & Tại sao?
Số phức, những số có một phần thực và một phần ảo (như 3 + 4i), rất quan trọng trong các lĩnh vực như kỹ thuật, vật lý, và khoa học dữ liệu. Các lập trình viên sử dụng chúng cho mô phỏng, xử lý tín hiệu, và giải quyết các loại bài toán toán học cụ thể.

## Làm thế nào:
PowerShell không hỗ trợ số phức một cách tích hợp, vì vậy bạn có thể tự tạo giải pháp của mình hoặc sử dụng `System.Numerics.Complex` của .NET.

```PowerShell
# Hãy tạo số phức sử dụng .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Tạo số phức
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Cộng hai số phức
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Nhân hai số phức
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Hiển thị kết quả
"Tổng: $sum"
"Tích: $product"
```
Kết quả:
```
Tổng: (4, 6)
Tích: (-5, 10)
```

## Sâu hơn
Số phức được phát triển vào thế kỷ 16 để giải quyết các phương trình không có giải pháp trong lĩnh vực số thực. Chúng giờ đây là nền tảng của toán học hiện đại.

Sự phụ thuộc vào .NET của PowerShell cho hỗ trợ số phức có nghĩa là hiệu suất là vững chắc. Các phương án thay thế bao gồm thư viện của bên thứ ba hoặc ngôn ngữ lập trình khác như Python, nơi số phức là một kiểu dữ liệu nguyên bản.

## Xem thêm
- [Cấu trúc System.Numerics.Complex](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Toán học với Số Phức trong Python](https://docs.python.org/3/library/cmath.html)
