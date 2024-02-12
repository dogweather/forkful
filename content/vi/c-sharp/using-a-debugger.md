---
title:                "Sử dụng bộ gỡ lỗi"
aliases:
- vi/c-sharp/using-a-debugger.md
date:                  2024-01-28T22:09:03.444332-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng bộ gỡ lỗi"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Sử dụng debugger (trình gỡ rối) có nghĩa là tận dụng các công cụ chuyên biệt để kiểm tra và chẩn đoán code. Các lập trình viên làm việc này để tìm và sửa lỗi, hiểu luồng code, và đảm bảo code của họ hoạt động như mong đợi - giống như việc bạn có một kính hiển vi cho bộ não của code.

## Cách thức:
Hãy tưởng tượng bạn có một chương trình nhỏ không hoạt động đúng như mong đợi:

```C#
static void Main()
{
    int result = Sum(1, 2);
    Console.WriteLine(result);
}

static int Sum(int a, int b)
{
    return a + a; // Ồ, phải là a + b
}
```

Sử dụng trình gỡ rối của Visual Studio, đặt một điểm dừng (breakpoint) bằng cách nhấp vào mép bên trái cạnh `return a + a;`. Khi bạn chạy chương trình (với F5), việc thực thi sẽ tạm dừng tại đó. Di chuột qua các biến để kiểm tra giá trị của chúng, hoặc sử dụng Immediate Window để đánh giá các biểu thức. Bạn sẽ thấy `a` là 1 và `b` là 2, nhưng `a + a` không phải là tổng chúng ta mong đợi. Thay đổi nó thành `a + b`, tiếp tục chạy (F5), và hoàn hảo, bảng điều khiển đầu ra 3.

## Sâu hơn
Lịch sử của việc gỡ rối trải dài từ thập kỷ 1940 khi một con bọ thực sự (một con bướm đêm) được tìm thấy trong một máy tính đời đầu. Công cụ gỡ rối ngày nay, như trình gỡ rối trong Visual Studio, cung cấp một loạt các tính năng mạnh mẽ, bao gồm điểm dừng, thực hiện từng bước, cửa sổ theo dõi, và nhiều hơn nữa.

Các lựa chọn thay thế cho trình gỡ rối của Visual Studio bao gồm các tùy chọn mã nguồn mở như GDB cho các ngôn ngữ kiểu C hoặc pdb cho Python, và các IDE đa nền tảng như JetBrains Rider hoặc VS Code, cung cấp công cụ gỡ rối cho C# và các ngôn ngữ khác.

Khi bạn lặn sâu vào việc thực hiện của một trình gỡ rối, bạn đang nhìn vào một chương trình gắn vào quy trình của ứng dụng của bạn. Nó giải thích mã máy, quản lý trạng thái bộ nhớ, và điều khiển luồng thực thi. Đây là những công việc nặng nhọc nhưng rất quan trọng cho việc gỡ rối hiệu quả, đó là lý do tại sao chế độ debug thường chạy chậm hơn chế độ release, nơi những móc nối này không tồn tại.

## Xem thêm
- [Tài liệu Trình Gỡ Rối Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/)
- [Các Chiến Lược Gỡ Rối](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
