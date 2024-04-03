---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:31.931231-07:00
description: "L\xE0m th\u1EBF n\xE0o: PowerShell kh\xF4ng h\u1ED7 tr\u1EE3 s\u1ED1\
  \ ph\u1EE9c m\u1ED9t c\xE1ch t\xEDch h\u1EE3p, v\xEC v\u1EADy b\u1EA1n c\xF3 th\u1EC3\
  \ t\u1EF1 t\u1EA1o gi\u1EA3i ph\xE1p c\u1EE7a m\xECnh ho\u1EB7c s\u1EED d\u1EE5\
  ng `System.Numerics.Complex` c\u1EE7a\u2026"
lastmod: '2024-03-13T22:44:36.925330-06:00'
model: gpt-4-0125-preview
summary: "PowerShell kh\xF4ng h\u1ED7 tr\u1EE3 s\u1ED1 ph\u1EE9c m\u1ED9t c\xE1ch\
  \ t\xEDch h\u1EE3p, v\xEC v\u1EADy b\u1EA1n c\xF3 th\u1EC3 t\u1EF1 t\u1EA1o gi\u1EA3\
  i ph\xE1p c\u1EE7a m\xECnh ho\u1EB7c s\u1EED d\u1EE5ng `System.Numerics.Complex`\
  \ c\u1EE7a .NET."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

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
