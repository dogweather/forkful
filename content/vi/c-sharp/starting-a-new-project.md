---
title:                "Bắt đầu một dự án mới"
date:                  2024-01-28T22:08:13.035416-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/c-sharp/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Bắt đầu một dự án C# mới có nghĩa là thiết lập một giải pháp mới từ đầu và các tập tin dự án để cấu trúc mã của bạn. Lập trình viên bắt đầu những dự án mới để biến ý tưởng thành phần mềm, giải quyết vấn đề, hoặc khám phá công nghệ.

## Cách thực hiện:
Hãy lăn tay áo lên và bắt tay vào công việc với một số mã lệnh. Giả sử bạn đã có .NET 6 hoặc phiên bản mới hơn - đó là phiên bản mới nhất tại thời điểm viết. Bạn sẽ sử dụng .NET CLI cho việc này.

Tạo một ứng dụng mới từ bảng điều khiển:
```C#
dotnet new console -o MyNewProject
```
Chuyển vào thư mục dự án của bạn:
```C#
cd MyNewProject
```
Chạy chương trình Hello World mới, cơ bản:
```C#
dotnet run
```
Bạn sẽ thấy:
```
Hello, World!
```
Dự án mới của bạn đã được khởi động!

## Sâu hơn
Ngày xưa, bạn có thể khởi động Visual Studio và click qua một trình hướng dẫn. Nhưng giờ đây, không còn nữa - .NET CLI là sự lựa chọn hàng đầu. Nó nhanh chóng và không giả định nhiều về môi trường phát triển của bạn.

Có sự thay thế không? Chắc chắn rồi. Visual Studio vẫn ở đó cho trải nghiệm giao diện người dùng đồ họa. Rider và Visual Studio Code cũng là những lựa chọn tốt. Nhưng CLI? Nó tất cả về cảm giác lập trình script gọn gàng, mạnh mẽ.

Chi tiết thực hiện? Tập tin `.csproj` của bạn giữ những chìa khóa của vương quốc. Nó là XML, nhưng đừng lo - nó tự xử lý phần lớn. Ở đây có các thông tin mà quá trình xây dựng của bạn cần - khung công tác mục tiêu, phụ thuộc, tham chiếu dự án, tất cả những thứ tốt lành.

## Xem thêm
- [Tài liệu chính thức của .NET CLI](https://docs.microsoft.com/en-us/dotnet/core/tools/)
- [Trang sản phẩm Visual Studio](https://visualstudio.microsoft.com/)
- [Tổng quan về .NET Project SDK](https://docs.microsoft.com/en-us/dotnet/core/project-sdk/overview)
