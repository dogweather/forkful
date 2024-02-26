---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:42.511386-07:00
description: "In th\xF4ng tin g\u1EE1 l\u1ED7i l\xE0 v\u1EC1 vi\u1EC7c xu\u1EA5t th\xF4\
  ng tin quan tr\u1ECDng \u0111\u1EC3 hi\u1EC3u \u0111\u01B0\u1EE3c nh\u1EEFng g\xEC\
  \ \u0111ang di\u1EC5n ra b\xEAn d\u01B0\u1EDBi c\u1EE7a m\xE3 l\u1EC7nh. C\xE1c\
  \ l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 theo\u2026"
lastmod: '2024-02-25T18:49:35.003874-07:00'
model: gpt-4-0125-preview
summary: "In th\xF4ng tin g\u1EE1 l\u1ED7i l\xE0 v\u1EC1 vi\u1EC7c xu\u1EA5t th\xF4\
  ng tin quan tr\u1ECDng \u0111\u1EC3 hi\u1EC3u \u0111\u01B0\u1EE3c nh\u1EEFng g\xEC\
  \ \u0111ang di\u1EC5n ra b\xEAn d\u01B0\u1EDBi c\u1EE7a m\xE3 l\u1EC7nh. C\xE1c\
  \ l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 theo\u2026"
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
In thông tin gỡ lỗi là về việc xuất thông tin quan trọng để hiểu được những gì đang diễn ra bên dưới của mã lệnh. Các lập trình viên làm điều này để theo dõi giá trị của biến, dòng chảy của quá trình thực thi, và dò tìm lỗi—giống như một dấu vết trong một khu rừng số hóa.

## Cách thực hiện:
Việc này khá đơn giản: sử dụng `Console.WriteLine()` để in ra bảng điều khiển xuất. Đặc biệt cho mục đích gỡ lỗi, `Debug.WriteLine()` có thể là công cụ bạn nên dùng, miễn là bạn đã có `System.Diagnostics` trong các chỉ thị sử dụng của mình. Nếu bạn đang hướng đến một ứng dụng UI, `Trace.WriteLine()` có thể là công cụ phù hợp vì nó cho phép người nghe thu được thông tin xuất.

```C#
using System;
using System.Diagnostics;

public class DebugExample
{
    public static void Main()
    {
        int magicNumber = 42;
        Console.WriteLine("Xin chào mọi người! Hãy cùng gỡ lỗi.");
        Debug.WriteLine($"Số thần kỳ là: {magicNumber}");

        // Giả sử chúng ta có một điều kiện ở đây
        Trace.WriteLine("Chúng ta đang trong ma trận!");
    }
}
```

Kết quả xuất trên bảng điều khiển sẽ nhìn như sau:
```
Xin chào mọi người! Hãy cùng gỡ lỗi.
```

Kết quả gỡ lỗi, hiển thị trong cửa sổ xuất gỡ lỗi của IDE hoặc người nghe, sẽ là:
```
Số thần kỳ là: 42
Chúng ta đang trong ma trận!
```

## Tìm hiểu sâu hơn
Hãy quay ngược thời gian. Khi C# còn mới, mọi người gỡ lỗi bằng hộp thoại thông báo—hãy tưởng tượng việc nhấn 'OK' hàng trăm lần. Nhưng công cụ phát triển. Phương thức 'Console.WriteLine()' là một cách nhanh chóng, đáng tin cậy để in ra thông tin, tốt nhất khi sử dụng trong các ứng dụng bảng điều khiển. Tuy nhiên, khi bạn đã chuyển từ việc phát triển ứng dụng bảng điều khiển sang phát triển ứng dụng Windows Forms hoặc WPF, ví dụ, 'Debug.WriteLine()' và 'Trace.WriteLine()' từ không gian tên `System.Diagnostics` trở nên hấp dẫn hơn.

'Debug.Writeline()' chỉ xuất thông tin khi build ở chế độ Debug; nó im lặng ở chế độ Release. Hành vi này làm cho nó gọn gàng cho các in gỡ lỗi tạm thời mà bạn không lo lắng về việc dọn dẹp sau này. Ngược lại, 'Trace.WriteLine()' có thể được bật cho cả build Debug và Release, có thể giúp theo dõi vấn đề sau khi triển khai.

Đáng chú ý là bạn có thể rải các lời gọi `Debug` và `Trace` xuyên suốt mã lệnh của mình, và bạn có thể điều khiển đầu ra của chúng bằng cách sử dụng Người nghe, mà không cần biên dịch lại mỗi lần bạn thay đổi nơi đầu ra đi. Thật tuyệt, phải không?

## Xem thêm
Để biết thêm những phút giây vui vẻ và kiến thức, hãy xem những liên kết này:
- Tài liệu chính thức của Microsoft về `Debug`: [Lớp Debug (System.Diagnostics)](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug)
- Tài liệu chính thức của Microsoft về `Trace`: [Lớp Trace (System.Diagnostics)](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.trace)
- Thảo luận sâu về người nghe và nguồn theo dõi: [Người Nghe Theo Dõi](https://docs.microsoft.com/en-us/dotnet/framework/debug-trace-profile/trace-listeners)
