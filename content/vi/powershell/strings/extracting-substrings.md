---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:17.861084-07:00
description: "Tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 l\u1EA5\
  y ra c\xE1c ph\u1EA7n nh\u1ECF h\u01A1n t\u1EEB m\u1ED9t chu\u1ED7i d\u1EF1a tr\xEA\
  n m\u1ED9t s\u1ED1 ti\xEAu ch\xED nh\u1EA5t \u0111\u1ECBnh. C\xE1c l\u1EADp tr\xEC\
  nh vi\xEAn tr\xEDch xu\u1EA5t chu\u1ED7i con \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.918658-06:00'
model: gpt-4-0125-preview
summary: "Tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 l\u1EA5\
  y ra c\xE1c ph\u1EA7n nh\u1ECF h\u01A1n t\u1EEB m\u1ED9t chu\u1ED7i d\u1EF1a tr\xEA\
  n m\u1ED9t s\u1ED1 ti\xEAu ch\xED nh\u1EA5t \u0111\u1ECBnh."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Cách thực hiện:
Dưới đây là cách để cắt và chia các chuỗi trong PowerShell:

```PowerShell
# Cho một chuỗi
$text = "Power up your PowerShell skills!"

# Trích xuất sử dụng phương thức substring
$startIndex = 10
$length = 9
$substring = $text.Substring($startIndex, $length)
Write-Host $substring  # Đầu ra: your Powe

# Trích xuất sử dụng toán tử range
$subrange = $text[10..18] -join ''
Write-Host $subrange  # Đầu ra: your Powe

# Trích xuất từ đầu cho đến một vị trí nào đó
$firstPart = $text.Substring(0, $startIndex)
Write-Host $firstPart  # Đầu ra: Power up 

# Trích xuất sau một ký tự nhất định
$splitString = $text.Split(" ")[2]
Write-Host $splitString  # Đầu ra: your
```

## Sâu hơn
Ngày xưa, PowerShell chỉ có những phương thức xử lý chuỗi cơ bản. Giờ đây, mọi thứ đã khác. Phương thức `.Substring()` đã tồn tại từ lâu và khá dễ hiểu—chỉ cần đưa cho nó chỉ số bắt đầu và một độ dài tùy chọn, và nó sẽ cắt ra những gì bạn cần. Bắt đầu từ PowerShell 6, bạn cũng có thể sử dụng toán tử range, có thể đơn giản hơn, đặc biệt khi bạn xử lý các chuỗi có độ dài biến đổi.

Còn có toán tử `-split` và phương thức `.Split()`, cả hai đều hữu ích để chia các chuỗi dựa trên mẫu hoặc ký tự. Cần một phần cụ thể? Sử dụng những công cụ này.

Về hiệu suất, không có nhiều khác biệt cho những tác vụ nhỏ. Khi bạn làm việc với các tệp văn bản khổng lồ hoặc lặp lại mỗi mili giây, bạn sẽ cần đến các chỉ số hiệu suất. Còn lại, đó nhiều hơn là về tính dễ đọc và cảm giác đúng đắn cho script của bạn.

Nhớ rằng, chuỗi trong PowerShell được đánh chỉ số bắt đầu từ không—điều này phổ biến trong nhiều ngôn ngữ lập trình. Hãy cẩn thận với lỗi off-by-one.

## Xem thêm
Để biết thêm về thao tác chuỗi trong PowerShell:

- [About_Split](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_split?view=powershell-7)
- [About Comparison Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7) bao gồm -split
