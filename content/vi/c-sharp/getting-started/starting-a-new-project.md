---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:13.035416-07:00
description: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n C# m\u1EDBi c\xF3 ngh\u0129\
  a l\xE0 thi\u1EBFt l\u1EADp m\u1ED9t gi\u1EA3i ph\xE1p m\u1EDBi t\u1EEB \u0111\u1EA7\
  u v\xE0 c\xE1c t\u1EADp tin d\u1EF1 \xE1n \u0111\u1EC3 c\u1EA5u tr\xFAc m\xE3 c\u1EE7\
  a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn b\u1EAFt \u0111\u1EA7u nh\u1EEFng d\u1EF1\
  \ \xE1n\u2026"
lastmod: '2024-03-13T22:44:36.658493-06:00'
model: gpt-4-0125-preview
summary: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n C# m\u1EDBi c\xF3 ngh\u0129\
  a l\xE0 thi\u1EBFt l\u1EADp m\u1ED9t gi\u1EA3i ph\xE1p m\u1EDBi t\u1EEB \u0111\u1EA7\
  u v\xE0 c\xE1c t\u1EADp tin d\u1EF1 \xE1n \u0111\u1EC3 c\u1EA5u tr\xFAc m\xE3 c\u1EE7\
  a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn b\u1EAFt \u0111\u1EA7u nh\u1EEFng d\u1EF1\
  \ \xE1n\u2026"
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
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
