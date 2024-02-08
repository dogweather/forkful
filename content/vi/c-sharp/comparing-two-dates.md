---
title:                "So sánh hai ngày"
date:                  2024-01-28T21:56:59.302913-07:00
model:                 gpt-4-0125-preview
simple_title:         "So sánh hai ngày"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
