---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:03.444332-07:00
description: "H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng b\u1EA1n c\xF3 m\u1ED9t ch\u01B0\
  \u01A1ng tr\xECnh nh\u1ECF kh\xF4ng ho\u1EA1t \u0111\u1ED9ng \u0111\xFAng nh\u01B0\
  \ mong \u0111\u1EE3i: ```C# static void Main() { int result = Sum(1, 2);\u2026"
lastmod: '2024-03-13T22:44:36.663563-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng b\u1EA1n c\xF3 m\u1ED9t ch\u01B0\u01A1\
  ng tr\xECnh nh\u1ECF kh\xF4ng ho\u1EA1t \u0111\u1ED9ng \u0111\xFAng nh\u01B0 mong\
  \ \u0111\u1EE3i: ```C# static void Main() { int result = Sum(1, 2);\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
weight: 35
---

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
