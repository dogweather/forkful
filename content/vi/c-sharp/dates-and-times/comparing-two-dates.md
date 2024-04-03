---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:59.302913-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y c\xF9ng nhau t\xECm hi\u1EC3u v\u1EC1\
  \ C# cho vi\u1EC7c so s\xE1nh ng\xE0y. Gi\u1EA3 s\u1EED ch\xFAng ta c\xF3 hai \u0111\
  \u1ED1i t\u01B0\u1EE3ng `DateTime`, `date1` v\xE0 `date2`. Ch\xFAng t\xF4i so s\xE1\
  nh\u2026"
lastmod: '2024-03-13T22:44:36.673836-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y c\xF9ng nhau t\xECm hi\u1EC3u v\u1EC1 C# cho vi\u1EC7c so s\xE1nh\
  \ ng\xE0y."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Cách thực hiện:
Hãy cùng nhau tìm hiểu về C# cho việc so sánh ngày. Giả sử chúng ta có hai đối tượng `DateTime`, `date1` và `date2`. Chúng tôi so sánh bằng cách dùng `DateTime.Compare(date1, date2)`, `date1.CompareTo(date2)`, hoặc so sánh trực tiếp các thuộc tính:

```C#
DateTime date1 = new DateTime(2023, 3, 25);
DateTime date2 = new DateTime(2023, 3, 30);

// Sử dụng phương thức tĩnh DateTime.Compare
int result = DateTime.Compare(date1, date2);

if(result < 0)
    Console.WriteLine("date1 sớm hơn date2.");
else if(result == 0)
    Console.WriteLine("date1 cùng thời điểm với date2.");
else
    Console.WriteLine("date1 muộn hơn date2.");

// Sử dụng phương thức thể hiện CompareTo
result = date1.CompareTo(date2);

if(result < 0)
    Console.WriteLine("date1 lại sớm hơn.");
else if(result == 0)
    Console.WriteLine("Vẫn là cùng một thời điểm?");
else
    Console.WriteLine("Lần này date1 đã muộn hơn?");

// So sánh trực tiếp
if(date1 < date2)
    Console.WriteLine("Yep, date1 sớm hơn, chúng ta có thể thấy trực tiếp.");
else if(date1 == date2)
    Console.WriteLine("Bằng nhau, đơn giản và rõ ràng.");
else
    Console.WriteLine("Hay là date1 muộn hơn? Không, không phải lần này.");
```

Kết quả sẽ cho thấy `date1` sớm hơn `date2` trong tất cả các phép so sánh—bạn đang làm rõ điều hiển nhiên, nhưng đó là mục đích của các bản ghi.

## Sâu hơn nữa
So sánh DateTime đã là một phần của C# kể từ khi nó ra đời, quan trọng cho việc xử lý khái niệm về thời gian luôn quan trọng. Nội bộ, giá trị `DateTime` đại diện cho số lần nhấp từ nửa đêm, ngày 1 tháng 1, năm 0001, trong Common Language Runtime.

Muốn thử cái khác? Bạn có thể sử dụng `TimeSpan` để tìm sự khác biệt, hoặc thử nghiệm với NodaTime, một thư viện của Jon Skeet cho việc xử lý ngày và giờ phức tạp hơn.

Đây là một sự thật thú vị về kỹ thuật: các loại `DateTime` trong .NET có thể là `Unspecified`, `Utc`, hoặc `Local`. So sánh thời gian UTC với thời gian địa phương? Điều đó mang lại rắc rối. Luôn đảm bảo loại trùng khớp để tránh logic sai lệch!

## Xem thêm
Hãy đào sâu hoặc làm rõ các vấn đề với những tài liệu sau:

- Tài liệu DateTime của Microsoft: https://docs.microsoft.com/en-us/dotnet/api/system.datetime
- Thêm về DateTime.Kind: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.kind
- NodaTime, dành cho những người yêu thích đồng hồ: https://nodatime.org/
- TimeSpan cho sự khác biệt thời gian: https://docs.microsoft.com/en-us/dotnet/api/system.timespan
