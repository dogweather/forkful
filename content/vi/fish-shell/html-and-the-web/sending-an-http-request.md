---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:44.495514-07:00
description: "L\xE0m th\u1EBF n\xE0o: Fish kh\xF4ng c\xF3 c\xE1c l\u1EC7nh t\xEDch\
  \ h\u1EE3p s\u1EB5n \u0111\u1EC3 g\u1EEDi c\xE1c y\xEAu c\u1EA7u HTTP, nh\u01B0\
  ng b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng `curl` ngay t\u1EEB shell."
lastmod: '2024-03-13T22:44:37.206503-06:00'
model: gpt-4-0125-preview
summary: "Fish kh\xF4ng c\xF3 c\xE1c l\u1EC7nh t\xEDch h\u1EE3p s\u1EB5n \u0111\u1EC3\
  \ g\u1EEDi c\xE1c y\xEAu c\u1EA7u HTTP, nh\u01B0ng b\u1EA1n c\xF3 th\u1EC3 s\u1EED\
  \ d\u1EE5ng `curl` ngay t\u1EEB shell."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Làm thế nào:
Fish không có các lệnh tích hợp sẵn để gửi các yêu cầu HTTP, nhưng bạn có thể sử dụng `curl` ngay từ shell:

```Fish
curl http://api.example.com/data
```

Đối với một yêu cầu POST với dữ liệu JSON:

```Fish
curl -X POST -H "Loại-Nội-Dung: ứng dụng/json" -d '{"key":"value"}' http://api.example.com/data
```

Để lưu trữ phản hồi:

```Fish
set response (curl -X GET http://api.example.com/data)
```

Và đây là những gì bạn có thể thấy sau một yêu cầu GET:

```Fish
{
  "response": "Một số dữ liệu từ máy chủ"
}
```

## Sâu hơn
Trong lịch sử, các shell UNIX và Linux luôn hữu ích cho các nhiệm vụ mạng. Ngày xưa, các công cụ như `telnet` thường được sử dụng cho mục đích như vậy. Ngày nay, các chương trình tiện ích như `curl` và `wget` là lựa chọn hàng đầu. `curl` là một công cụ đa năng hỗ trợ nhiều giao thức, và thường được sử dụng vì sự đơn giản và linh hoạt của nó.

Python hoặc Node.js có thể được sử dụng khi bạn cần xử lý yêu cầu phức tạp hơn. Nhưng đối với các nhiệm vụ nhanh chóng hoặc các script đơn giản, `curl` trong Fish hiệu quả và có hiệu lực.

Thực hiện một yêu cầu HTTP thông qua Fish thường nghĩa là dựa vào các công cụ bên thứ ba. Fish được thiết kế để là một shell dòng lệnh thông minh và thân thiện với người dùng, không phải là một công cụ làm mọi việc. Khi bạn kết hợp nó với sức mạnh của các tiện ích như `curl`, bạn có được điều tốt nhất từ cả hai thế giới: sự dễ sử dụng của Fish và khả năng của `curl`.

## Xem thêm
- Tìm hiểu thêm về `curl`: https://curl.se/docs/manual.html
- Tài liệu về Shell Fish: https://fishshell.com/docs/current/index.html
- Tổng quan cơ bản về HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview
- Khám phá APIs với `httpie`, một lựa chọn thay thế cho `curl`: https://httpie.io/
