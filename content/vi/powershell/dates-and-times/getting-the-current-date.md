---
aliases:
- /vi/powershell/getting-the-current-date/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:35.428425-07:00
description: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong PowerShell ch\u1EC9 l\xE0\
  \ l\u1EA5y \xFD t\u01B0\u1EDFng c\u1EE7a h\u1EC7 th\u1ED1ng v\u1EC1 ng\xE0y h\xF4\
  m nay. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ \u0111\xE1nh d\u1EA5u th\u1EDDi gian trong c\xE1c b\u1EA3n\u2026"
lastmod: 2024-02-18 23:08:50.945674
model: gpt-4-0125-preview
summary: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong PowerShell ch\u1EC9 l\xE0 l\u1EA5\
  y \xFD t\u01B0\u1EDFng c\u1EE7a h\u1EC7 th\u1ED1ng v\u1EC1 ng\xE0y h\xF4m nay. L\u1EAD\
  p tr\xECnh vi\xEAn s\u1EED d\u1EE5ng \u0111i\u1EC1u n\xE0y \u0111\u1EC3 \u0111\xE1\
  nh d\u1EA5u th\u1EDDi gian trong c\xE1c b\u1EA3n\u2026"
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
---

{{< edit_this_page >}}

## Lý do & Vì sao?

Lấy ngày hiện tại trong PowerShell chỉ là lấy ý tưởng của hệ thống về ngày hôm nay. Lập trình viên sử dụng điều này để đánh dấu thời gian trong các bản ghi nhật ký, tính toán khoảng thời gian, hoặc kích hoạt các thao tác theo thời gian cụ thể.

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
