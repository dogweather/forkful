---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:48.996322-07:00
description: "Vi\u1EC7c t\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai\
  \ ho\u1EB7c qu\xE1 kh\u1EE9 ngh\u0129a l\xE0 x\xE1c \u0111\u1ECBnh xem ng\xE0y s\u1EBD\
  \ l\xE0 bao nhi\xEAu sau ho\u1EB7c tr\u01B0\u1EDBc m\u1ED9t kho\u1EA3ng th\u1EDD\
  i gian nh\u1EA5t \u0111\u1ECBnh. C\xE1c l\u1EADp\u2026"
lastmod: '2024-03-11T00:14:10.248989-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7\
  c qu\xE1 kh\u1EE9 ngh\u0129a l\xE0 x\xE1c \u0111\u1ECBnh xem ng\xE0y s\u1EBD l\xE0\
  \ bao nhi\xEAu sau ho\u1EB7c tr\u01B0\u1EDBc m\u1ED9t kho\u1EA3ng th\u1EDDi gian\
  \ nh\u1EA5t \u0111\u1ECBnh. C\xE1c l\u1EADp\u2026"
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc tính toán một ngày trong tương lai hoặc quá khứ nghĩa là xác định xem ngày sẽ là bao nhiêu sau hoặc trước một khoảng thời gian nhất định. Các lập trình viên làm việc này để tự động hóa việc nhắc nhở, lên lịch cho các nhiệm vụ, hoặc xử lý các ngày hết hạn.

## Làm thế nào:

### Thêm ngày vào ngày hiện tại:
```PowerShell
# Thêm 10 ngày vào ngày hôm nay
$newDate = (Get-Date).AddDays(10)
Write-Output $newDate
```

Kết quả mẫu:
```
Thứ Năm, 13 Tháng Tư, 2023
```

### Bớt ngày từ ngày hiện tại:
```PowerShell
# Bớt 15 ngày từ hôm nay
$pastDate = (Get-Date).AddDays(-15)
Write-Output $pastDate
```

Kết quả mẫu:
```
Thứ Tư, 20 Tháng Ba, 2023
```

### Tính toán sự chênh lệch giữa hai ngày:
```PowerShell
# Sự chênh lệch giữa hai ngày
$date1 = Get-Date '2023-04-01'
$date2 = Get-Date '2023-04-15'
$diff = $date2 - $date1
Write-Output $diff.Days
```

Kết quả mẫu:
```
14
```

## Tìm hiểu sâu hơn
Có một thời, các lập trình viên phải tự tính toán các ngày bằng cách sử dụng các thuật toán phức tạp. Giờ đây, các ngôn ngữ như PowerShell cung cấp các hàm tích hợp sẵn như `AddDays`, `AddMonths`, khiến việc này trở nên gần như không còn là vấn đề.

### Các phương án thay thế:
Mặc dù `AddDays` rất tiện lợi, nhưng cũng có các hàm như `AddHours`, `AddMinutes`, vv., cho phép kiểm soát tỉ mỉ hơn. Ngoài ra, bạn có thể sử dụng `[datetime]::Today.AddDays(10)` nếu bạn thích một cách tiếp cận tĩnh.

### Chi tiết triển khai:
Đối tượng `DateTime` của PowerShell đã tích hợp sẵn các phương thức này, vì vậy bạn không cần phải bắt đầu từ đầu. Bên dưới bề mặt, nó đang xử lý tất cả các loại phức tạp như năm nhuận và điều chỉnh ánh sáng ban ngày cho bạn.

## Xem thêm
- Tài liệu chính thức của PowerShell về các phương thức `DateTime`: [Microsoft Docs - Phương Thức DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- Thêm thông tin về phép toán ngày tháng trong PowerShell: [Phép Toán Ngày Tháng trong PowerShell](https://ss64.com/ps/syntax-dateformats.html)
- Để tìm hiểu sâu hơn về lịch sử và các tinh tế trong hệ thống lịch liên quan đến tính toán ngày: [Câu Hỏi Thường Gặp về Lịch](http://www.tondering.dk/claus/cal/calendar29.html)
