---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:21.946145-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: C\xF4ng c\u1EE5 ti\xEAu bi\u1EC3u cho c\xF4\
  ng vi\u1EC7c n\xE0y? `curl`. \u0110\xF3 l\xE0 m\u1ED9t ti\u1EC7n \xEDch d\xF2ng\
  \ l\u1EC7nh m\u1EA1nh m\u1EBD gi\xFAp t\u1EA3i d\u1EEF li\u1EC7u t\u1EEB web. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 tr\u01B0\u1EDDng h\u1EE3p s\u1EED d\u1EE5ng\u2026"
lastmod: '2024-04-05T22:40:25.048966-06:00'
model: gpt-4-0125-preview
summary: "C\xF4ng c\u1EE5 ti\xEAu bi\u1EC3u cho c\xF4ng vi\u1EC7c n\xE0y?"
title: "T\u1EA3i trang web"
weight: 42
---

## Cách thực hiện:
Công cụ tiêu biểu cho công việc này? `curl`. Đó là một tiện ích dòng lệnh mạnh mẽ giúp tải dữ liệu từ web. Dưới đây là trường hợp sử dụng đơn giản nhất:

```Bash
curl https://example.com -o webpage.html
```

Lệnh này tải HTML của `example.com` và ghi nó vào một tệp có tên là `webpage.html`. Kiểm tra đầu ra:

```Bash
# Đầu ra mẫu
  % Tổng cộng    % Đã nhận % Đã chuyển  Tốc độ trung bình   Thời gian   Thời gian     Thời gian  Hiện tại
                                 Tải xuống  Tải lên   Tổng cộng   Đã dùng    Còn lại  Tốc độ
100  1256  100  1256    0     0   6458      0 --:--:-- --:--:-- --:--:--  6497
```

Muốn xem bạn đang tải gì trong thời gian thực không? Bỏ qua `-o` và việc tải xuống sẽ in ngay trong bảng điều khiển của bạn:

```Bash
curl https://example.com
```

## Sâu hơn
`curl` đã xuất hiện từ năm 1997, tạo chỗ đứng cho mình trong các thao tác web. Tại sao lại chọn `curl` thay vì tải xuống bằng trình duyệt? Tự động hóa và thân thiện với kịch bản. Nó không tương tác và có thể dễ dàng tích hợp vào các kịch bản bash.

Các lựa chọn thay thế đáng nhắc: `wget`, một công cụ dòng lệnh khác có khả năng tải xuống các trang web một cách đệ quy. Đối với các công việc scraping nặng nhọc hoặc khi cần một ngữ cảnh trình duyệt thực sự, các lập trình viên chuyển sang sử dụng các công cụ như Selenium, Puppeteer, hoặc Scrapy.

Tìm hiểu sâu hơn về cách hoạt động của `curl`: Nó hỗ trợ nhiều giao thức, từ HTTP và HTTPS đến FTP, cùng với một loạt các lựa chọn (--header, --cookie, --user-agent, v.v.) để tinh chỉnh các yêu cầu. Hơn nữa, nó thường đã được cài đặt sẵn trên các hệ thống dựa trên Unix.

## Tham khảo thêm
- Tài liệu Curl: https://curl.haxx.se/docs/manpage.html
- Hướng dẫn Wget: https://www.gnu.org/software/wget/manual/wget.html
- Giới thiệu về web scraping với Python: https://realpython.com/python-web-scraping-practical-introduction/
