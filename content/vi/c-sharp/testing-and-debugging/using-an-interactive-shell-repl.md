---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:02.622594-07:00
description: "C\xE1ch th\u1EE9c: Kh\u1EDFi \u0111\u1ED9ng REPL trong m\xF4i tr\u01B0\
  \u1EDDng C# c\u1EE7a b\u1EA1n s\u1EED d\u1EE5ng c\u1EEDa s\u1ED5 C# Interactive\
  \ ho\u1EB7c ch\u1EA1y `dotnet-script` trong terminal c\u1EE7a b\u1EA1n. \u0110\xE2\
  y l\xE0 m\u1ED9t v\xED d\u1EE5 v\u1EC1\u2026"
lastmod: '2024-03-13T22:44:36.659756-06:00'
model: gpt-4-0125-preview
summary: "Kh\u1EDFi \u0111\u1ED9ng REPL trong m\xF4i tr\u01B0\u1EDDng C# c\u1EE7a\
  \ b\u1EA1n s\u1EED d\u1EE5ng c\u1EEDa s\u1ED5 C# Interactive ho\u1EB7c ch\u1EA1\
  y `dotnet-script` trong terminal c\u1EE7a b\u1EA1n."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

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
