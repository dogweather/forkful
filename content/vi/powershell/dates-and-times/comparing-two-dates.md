---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:25.860601-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ngay t\u1EEB th\u1EDDi k\u1EF3 \u0111\xE1 c\u1EE7\
  a m\xE1y t\xEDnh - kh\xF4ng h\u1EB3n v\u1EADy, nh\u01B0ng b\u1EA1n bi\u1EBFt \u0111\
  \u1EA5y, nh\u1EEFng ng\xE0y \u0111\u1EA7u - d\u1EEF li\u1EC7u ng\xE0y th\xE1ng r\u1EA5\
  t l\u1ED9n x\u1ED9n. Ch\xFAng ta \u0111\xE3 ti\u1EBFn xa v\u1EDBi\u2026"
lastmod: '2024-04-05T21:53:38.315969-06:00'
model: gpt-4-0125-preview
summary: "Ngay t\u1EEB th\u1EDDi k\u1EF3 \u0111\xE1 c\u1EE7a m\xE1y t\xEDnh - kh\xF4\
  ng h\u1EB3n v\u1EADy, nh\u01B0ng b\u1EA1n bi\u1EBFt \u0111\u1EA5y, nh\u1EEFng ng\xE0\
  y \u0111\u1EA7u - d\u1EEF li\u1EC7u ng\xE0y th\xE1ng r\u1EA5t l\u1ED9n x\u1ED9n."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Làm thế nào:
```PowerShell
# Lấy ngày hôm nay
$today = Get-Date

# Và đây là một ngày bất kỳ
$someOtherDate = Get-Date "2023-03-17"

# Chúng có bằng nhau không?
$today -eq $someOtherDate

# Hôm nay có phải lớn hơn (muộn hơn) ngày kia không?
$today -gt $someOtherDate

# Còn kiểm tra xem nó có sớm hơn không?
$today -lt $someOtherDate

# Chúng ta hãy xem kết quả, nhé?

False
True
False
```

## Thảo Luận Sâu
Ngay từ thời kỳ đá của máy tính - không hẳn vậy, nhưng bạn biết đấy, những ngày đầu - dữ liệu ngày tháng rất lộn xộn. Chúng ta đã tiến xa với các tiêu chuẩn và PowerShell làm cho nó đơn giản hơn nữa.

Dưới đây là những điều đáng lưu ý:
1. **Lịch sử**: Trước đây, máy tính sử dụng nhiều định dạng ngày khác nhau, dẫn đến sự nhầm lẫn và các lỗi kiểu Y2K. PowerShell dựa vào cấu trúc `DateTime` của .NET, tránh được sự hỗn loạn đó.

2. **Các lựa chọn khác**: Bạn cũng có thể sử dụng `Compare-Object`, hoặc tận dụng các phương pháp từ đối tượng `[datetime]` như `.AddDays()` để thực hiện các phép toán trước khi so sánh. Nhớ sử dụng `Measure-Command` để kiểm tra ảnh hưởng hiệu suất.

3. **Chi tiết triển khai**: Ngày trong PowerShell là các đối tượng với các thuộc tính và phương pháp của riêng chúng. So sánh ngày được thực hiện với các toán tử (`-eq`, `-lt`, `-gt`), và, nhờ vào việc nạp chồng toán tử, PowerShell biết bạn đang xử lý với ngày, không phải chỉ là chuỗi hay số.

Ở cấp độ assembly, so sánh ngày được chuyển đổi thành ticks (khoảng cách 100-nanosecond kể từ 1/1/0001). Vì thế bạn thực chất đang so sánh các số nguyên lớn, đây là cách hiệu quả.

## Tham Khảo Thêm
- [Cấu trúc DateTime (Tài liệu Microsoft)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [Làm việc với Ngày và Giờ trong PowerShell (SS64.com)](https://ss64.com/ps/syntax-dateformats.html)
