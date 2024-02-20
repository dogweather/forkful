---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:59.302913-07:00
description: "So s\xE1nh hai ng\xE0y ngh\u0129a l\xE0 ki\u1EC3m tra ch\xFAng c\xF3\
  \ m\u1ED1i quan h\u1EC7 nh\u01B0 th\u1EBF n\xE0o\u2014m\u1ED9t ng\xE0y c\xF3 s\u1EDB\
  m h\u01A1n, mu\u1ED9n h\u01A1n, hay ch\xEDnh x\xE1c c\xF9ng m\u1ED9t th\u1EDDi \u0111\
  i\u1EC3m v\u1EDBi ng\xE0y kia kh\xF4ng.\u2026"
lastmod: 2024-02-19 22:04:55.849336
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y ngh\u0129a l\xE0 ki\u1EC3m tra ch\xFAng c\xF3 m\u1ED1\
  i quan h\u1EC7 nh\u01B0 th\u1EBF n\xE0o\u2014m\u1ED9t ng\xE0y c\xF3 s\u1EDBm h\u01A1\
  n, mu\u1ED9n h\u01A1n, hay ch\xEDnh x\xE1c c\xF9ng m\u1ED9t th\u1EDDi \u0111i\u1EC3\
  m v\u1EDBi ng\xE0y kia kh\xF4ng.\u2026"
title: "So s\xE1nh hai ng\xE0y"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

So sánh hai ngày nghĩa là kiểm tra chúng có mối quan hệ như thế nào—một ngày có sớm hơn, muộn hơn, hay chính xác cùng một thời điểm với ngày kia không. Lập trình viên làm việc này để xử lý lịch trình, xác minh tuổi, kích hoạt sự kiện, và nhiều hơn nữa—cơ bản là bất kỳ khi nào chúng ta cần đo lường sự khác biệt thời gian hoặc sắp xếp các sự kiện theo trình tự.

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
