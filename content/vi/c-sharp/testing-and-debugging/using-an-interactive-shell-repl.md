---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:02.622594-07:00
description: "REPL, hay V\xF2ng L\u1EB7p \u0110\u1ECDc-\u0110\xE1nh gi\xE1-In, cho\
  \ ph\xE9p b\u1EA1n nh\u1EADp m\xE3 C# v\xE0 ch\u1EA1y n\xF3 m\u1ED9t c\xE1ch t\u01B0\
  \u01A1ng t\xE1c. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\
  \u1EC3 th\u1EED nghi\u1EC7m nhanh, g\u1EE1 l\u1ED7i, ho\u1EB7c\u2026"
lastmod: '2024-03-13T22:44:36.659756-06:00'
model: gpt-4-0125-preview
summary: "REPL, hay V\xF2ng L\u1EB7p \u0110\u1ECDc-\u0110\xE1nh gi\xE1-In, cho ph\xE9\
  p b\u1EA1n nh\u1EADp m\xE3 C# v\xE0 ch\u1EA1y n\xF3 m\u1ED9t c\xE1ch t\u01B0\u01A1\
  ng t\xE1c."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Cái gì và Tại sao?
REPL, hay Vòng Lặp Đọc-Đánh giá-In, cho phép bạn nhập mã C# và chạy nó một cách tương tác. Các lập trình viên sử dụng nó để thử nghiệm nhanh, gỡ lỗi, hoặc học C#, mà không cần phải thiết lập các dự án đầy đủ.

## Cách thức:
Khởi động REPL trong môi trường C# của bạn sử dụng cửa sổ C# Interactive hoặc chạy `dotnet-script` trong terminal của bạn. Đây là một ví dụ về việc sử dụng nó:

```csharp
> var greeting = "Xin chào, REPL!";
> Console.WriteLine(greeting);
Xin chào, REPL!
> 
```

Bạn nhận được phản hồi ngay lập tức. Không cần bước biên dịch và chạy. Chỉ viết mã và xem kết quả.

## Sâu hơn
REPL đã đi qua từ Lisp đến các ngôn ngữ hiện đại, phát triển mạnh mẽ trong các ngôn ngữ động như Python. Với C#, Roslyn đã đưa REPL gần hơn với các nhà phát triển. `csi` cho Roslyn, và `dotnet-script` cho .NET Core, là những lựa chọn vững chắc. Một cái nhìn sâu hơn: chúng đánh giá mã theo từng dòng, không phải tất cả cùng một lúc, một mô hình thực thi khác biệt so với các ứng dụng C# thông thường. Điều này ảnh hưởng đến việc bảo toàn trạng thái qua các lần thực thi và phạm vi của biến.

Cửa sổ C# Interactive của Visual Studio là một REPL được cung cấp bởi Roslyn. Nó có Intellisense, nhiều tham chiếu, và hỗ trợ gói NuGet. Một bước tiến lớn so với các thí nghiệm dòng lệnh đầu tiên.

Đối với các ngôn ngữ khác, Python sử dụng `IDLE`, JavaScript có REPL của Node.js, và F# đi kèm với `F# Interactive`. Mỗi ngôn ngữ tạo điều kiện cho vòng lặp phản hồi tức thì, vô giá trong việc kiểm tra các đoạn mã nhỏ hoặc hiểu các tính năng ngôn ngữ.

## Xem thêm
- [REPL `dotnet-script` của .NET Core](https://github.com/filipw/dotnet-script)
