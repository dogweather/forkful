---
title:                "Sử dụng vỏ tương tác (REPL)"
date:                  2024-01-28T22:09:02.622594-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
