---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:03.205850-07:00
description: "L\xE0m tr\xF2n s\u1ED1 l\xE0 vi\u1EC7c \u0111i\u1EC1u ch\u1EC9nh gi\xE1\
  \ tr\u1ECB v\u1EC1 s\u1ED1 nguy\xEAn g\u1EA7n nh\u1EA5t ho\u1EB7c v\u1ECB tr\xED\
  \ th\u1EADp ph\xE2n \u0111\u01B0\u1EE3c ch\u1EC9 \u0111\u1ECBnh. L\u1EADp tr\xEC\
  nh vi\xEAn th\u01B0\u1EDDng l\xE0m tr\xF2n s\u1ED1 \u0111\u1EC3 \u0111\u01A1n gi\u1EA3\
  n h\xF3a d\u1EEF\u2026"
lastmod: '2024-03-13T22:44:36.926617-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m tr\xF2n s\u1ED1 l\xE0 vi\u1EC7c \u0111i\u1EC1u ch\u1EC9nh gi\xE1\
  \ tr\u1ECB v\u1EC1 s\u1ED1 nguy\xEAn g\u1EA7n nh\u1EA5t ho\u1EB7c v\u1ECB tr\xED\
  \ th\u1EADp ph\xE2n \u0111\u01B0\u1EE3c ch\u1EC9 \u0111\u1ECBnh. L\u1EADp tr\xEC\
  nh vi\xEAn th\u01B0\u1EDDng l\xE0m tr\xF2n s\u1ED1 \u0111\u1EC3 \u0111\u01A1n gi\u1EA3\
  n h\xF3a d\u1EEF\u2026"
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Cái gì & Tại sao?
Làm tròn số là việc điều chỉnh giá trị về số nguyên gần nhất hoặc vị trí thập phân được chỉ định. Lập trình viên thường làm tròn số để đơn giản hóa dữ liệu, tăng cường khả năng đọc hoặc đáp ứng một số yêu cầu toán học nhất định trong quá trình tính toán.

## Làm thế nào:
Bạn có một số cmdlet và phương thức hữu ích trong PowerShell để làm tròn:

- Phương thức `Round()` từ lớp Math
```PowerShell
[Math]::Round(15.68) # Làm tròn thành 16
```
- Chỉ định số thập phân:
```PowerShell
[Math]::Round(15.684, 2) # Làm tròn thành 15.68
```
- `Ceiling()` và `Floor()`, luôn làm tròn lên hoặc xuống:
```PowerShell
[Math]::Ceiling(15.2) # Làm tròn lên thành 16
[Math]::Floor(15.9) # Làm tròn xuống thành 15
```

## Sâu hơn
Làm tròn số không phải là điều gì mới mẻ; nó đã tồn tại từ thời cổ đại, hữu ích cho việc thương mại, khoa học, và đo lường thời gian. Nói về PowerShell, `[Math]::Round()` mặc định tuân theo "Làm tròn của Ngân Hàng", nơi mà 0.5 sẽ được làm tròn về số chẵn gần nhất, giảm thiên vị trong các hoạt động thống kê.

Bạn không chỉ mắc kẹt với các phương pháp `[Math]` thôi đâu. Muốn kiểm soát nhiều hơn? Hãy xem `[System.Math]::Round(Số, Chữ số, MidpointRounding)` nơi bạn có thể thiết lập cách xử lý điểm giữa: xa số không hoặc về số chẵn (còn gọi là Làm tròn của Ngân Hàng).

Một góc nhìn khác: đối tượng `System.Globalization.CultureInfo`. Nó giúp với việc định dạng phù hợp theo địa phương và ưu tiên làm tròn khi xử lý số liệu quốc tế.

## Xem thêm
- Tài liệu chính thức của Microsoft về các phương pháp Math: [Liên kết](https://learn.microsoft.com/en-us/dotnet/api/system.math?view=net-7.0)
- Chi tiết về làm tròn số thập phân trong .NET: [Liên kết](https://learn.microsoft.com/en-us/dotnet/api/system.midpointrounding?view=net-7.0)
- Thảo luận về việc làm tròn trên StackOverflow: [Liên kết](https://stackoverflow.com/questions/tagged/rounding+powershell)
