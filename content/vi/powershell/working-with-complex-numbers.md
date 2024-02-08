---
title:                "Làm việc với số phức"
aliases:
- vi/powershell/working-with-complex-numbers.md
date:                  2024-01-28T22:12:31.931231-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
