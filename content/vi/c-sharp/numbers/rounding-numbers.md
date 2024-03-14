---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:35.254787-07:00
description: "L\xE0m tr\xF2n s\u1ED1 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9nh ch\xFA\
  ng v\u1EC1 gi\xE1 tr\u1ECB \u0111\u1ECBa \u0111i\u1EC3m g\u1EA7n nh\u1EA5t\u2014\
  ngh\u0129 v\u1EC1 vi\u1EC7c \u0111\u01A1n gi\u1EA3n h\xF3a ch\xFAng. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m tr\xF2n s\u1ED1 \u0111\u1EC3 ki\u1EC3m so\xE1t \u0111\u1ED9\
  \ ch\xEDnh\u2026"
lastmod: '2024-03-13T22:44:36.650653-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m tr\xF2n s\u1ED1 ngh\u0129a l\xE0 \u0111i\u1EC1u ch\u1EC9nh ch\xFA\
  ng v\u1EC1 gi\xE1 tr\u1ECB \u0111\u1ECBa \u0111i\u1EC3m g\u1EA7n nh\u1EA5t\u2014\
  ngh\u0129 v\u1EC1 vi\u1EC7c \u0111\u01A1n gi\u1EA3n h\xF3a ch\xFAng. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m tr\xF2n s\u1ED1 \u0111\u1EC3 ki\u1EC3m so\xE1t \u0111\u1ED9\
  \ ch\xEDnh\u2026"
title: "L\xE0m tr\xF2n s\u1ED1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Làm tròn số nghĩa là điều chỉnh chúng về giá trị địa điểm gần nhất—nghĩ về việc đơn giản hóa chúng. Các lập trình viên làm tròn số để kiểm soát độ chính xác, tăng hiệu suất, hoặc khi muốn hiển thị kết quả thân thiện với người dùng—như những giá cả không cần ba số thập phân.

## Cách thực hiện:
Dưới đây là vé khứ hồi cho việc làm tròn số trong C#:

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double soGoc = 123.4567;

        // Làm tròn về số nguyên gần nhất
        double lamTron = Math.Round(soGoc);
        Console.WriteLine(lamTron); // Kết quả: 123

        // Chỉ định số chữ số sau dấu phẩy
        double lamTronHaiSoThapPhan = Math.Round(soGoc, 2);
        Console.WriteLine(lamTronHaiSoThapPhan); // Kết quả: 123.46

        // Làm tròn lên bất kể chữ số tiếp theo
        double lamTronLen = Math.Ceiling(soGoc);
        Console.WriteLine(lamTronLen); // Kết quả: 124

        // Làm tròn xuống bất kể chữ số tiếp theo
        double lamTronXuong = Math.Floor(soGoc);
        Console.WriteLine(lamTronXuong); // Kết quả: 123
    }
}
```

## Sâu hơn nữa
Ngày xửa ngày xưa, làm tròn số là việc rất dễ dàng để cắt giảm chi phí tính toán. Mỗi chu kỳ đếm, và cắt giảm số lượng tiết kiệm thời gian quý báu. Nhanh chóng chuyển về C# hiện đại, và đó là về việc quản lý lỗi chính xác nổi tiếng và những điều kỳ lạ trong hiển thị của doubles và decimals.

Ngoài `Math.Round`, `Math.Floor`, và `Math.Ceiling`, enum `MidpointRounding` cho phép chúng ta quyết định số phận của những chữ số nằm ở giữa - đó là ngã tư giữa quy tắc của ngân hàng và công bằng của trò chơi "làm tròn nửa lên".

Đối với những đám đông khó tính, như các ứng dụng toán học hay tài chính nghiêm túc, chúng ta sử dụng `decimal` thay vì `double`, cắt giảm drama làm tròn bằng cách cung cấp độ chính xác cao hơn—ít làm tròn hơn, ít vấn đề hơn.

## Xem thêm
- [Tài liệu chính thức C# về `Math.Round`](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: Khi nào tôi nên sử dụng Double thay vì Decimal?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [Tiêu chuẩn IEEE cho Số học Dấu phẩy động (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754)
