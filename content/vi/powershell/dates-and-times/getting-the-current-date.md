---
title:                "Lấy ngày hiện tại"
aliases:
- /vi/powershell/getting-the-current-date.md
date:                  2024-01-28T22:01:35.428425-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
