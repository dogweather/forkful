---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:35.428425-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 \u0111\
  o\u1EA1n m\xE3 \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 l\u1EA5y ng\xE0y h\xF4m nay."
lastmod: '2024-03-13T22:44:36.950161-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 \u0111o\u1EA1n m\xE3 \u0111\u01A1n gi\u1EA3\
  n \u0111\u1EC3 l\u1EA5y ng\xE0y h\xF4m nay."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

## Cách thực hiện:
Dưới đây là đoạn mã đơn giản để lấy ngày hôm nay:

```PowerShell
Get-Date
```

Và ch voilà, kết quả:

```plaintext
Thứ Ba, 14 Tháng Ba, 2023 10:15:42 Sáng
```

Có thể bạn muốn cái gì đó cụ thể hơn, như chỉ lấy ngày:

```PowerShell
(Get-Date).Day
```
Kết quả:

```plaintext
14
```

Làm sao để chúng ta có thể quốc tế hóa? Lấy ngày theo định dạng ISO 8601:

```PowerShell
Get-Date -Format 'yyyy-MM-dd'
```

Kết quả:

```plaintext
2023-03-14
```

## Sâu hơn nữa
Trở lại thời kỳ trước, việc lấy ngày trong ngôn ngữ script không phải là chuyện nhỏ. Nhưng PowerShell, đã học hỏi từ sự phức tạp và nhu cầu của lịch sử computing, đã làm cho nó trở thành một dòng mã.

Ngoài `Get-Date`, các lựa chọn khác bao gồm việc đào sâu vào lớp .NET System.DateTime cho những nhu cầu phức tạp hơn, hoặc sử dụng WMI (Windows Management Instrumentation) để lấy thông tin hệ thống. Tuy nhiên, `Get-Date` là lựa chọn hàng đầu về sự đơn giản và hiệu quả.

Bên trong, `Get-Date` tận dụng đồng hồ hệ thống và các thiết lập khu vực của bạn để đảm bảo ngày và giờ được biểu diễn một cách chính xác, phản ánh các khu vực thời gian và điều chỉnh giờ mùa hè.

Nó còn có thể tùy chỉnh hoàn toàn. Bạn có thể tạo ra định dạng đầu ra theo ý thích của mình sử dụng các chuỗi định dạng tiêu chuẩn hoặc tùy chỉnh - một tính năng tiện lợi cho các bản ghi nhật ký cần tuân theo một số quy ước nhất định hoặc cho sự hài hòa về mặt hình thức trong đầu ra của bạn.

## Xem thêm
Dưới đây là một số tài nguyên để xem xét:

- [Tài liệu Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Cấu trúc DateTime của .NET](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Chuỗi định dạng ngày và giờ tùy chỉnh](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
