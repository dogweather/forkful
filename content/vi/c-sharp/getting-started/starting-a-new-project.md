---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:13.035416-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y l\u0103n tay \xE1o l\xEAn v\xE0\
  \ b\u1EAFt tay v\xE0o c\xF4ng vi\u1EC7c v\u1EDBi m\u1ED9t s\u1ED1 m\xE3 l\u1EC7\
  nh. Gi\u1EA3 s\u1EED b\u1EA1n \u0111\xE3 c\xF3 .NET 6 ho\u1EB7c phi\xEAn b\u1EA3\
  n m\u1EDBi h\u01A1n - \u0111\xF3 l\xE0 phi\xEAn b\u1EA3n m\u1EDBi nh\u1EA5t\u2026"
lastmod: '2024-03-13T22:44:36.658493-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y l\u0103n tay \xE1o l\xEAn v\xE0 b\u1EAFt tay v\xE0o c\xF4ng vi\u1EC7\
  c v\u1EDBi m\u1ED9t s\u1ED1 m\xE3 l\u1EC7nh."
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

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
