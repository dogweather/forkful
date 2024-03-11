---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:21.946145-07:00
description: "T\u1EA3i xu\u1ED1ng m\u1ED9t trang web c\xF3 ngh\u0129a l\xE0 l\u1EA5\
  y d\u1EEF li\u1EC7u t\u1EEB internet v\xE0 l\u01B0u tr\u1EEF c\u1EE5c b\u1ED9. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y cho c\xE1c m\u1EE5\
  c \u0111\xEDch nh\u01B0 web scraping, ph\xE2n\u2026"
lastmod: '2024-03-11T00:14:10.167304-06:00'
model: gpt-4-0125-preview
summary: "T\u1EA3i xu\u1ED1ng m\u1ED9t trang web c\xF3 ngh\u0129a l\xE0 l\u1EA5y d\u1EEF\
  \ li\u1EC7u t\u1EEB internet v\xE0 l\u01B0u tr\u1EEF c\u1EE5c b\u1ED9. L\u1EADp\
  \ tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y cho c\xE1c m\u1EE5\
  c \u0111\xEDch nh\u01B0 web scraping, ph\xE2n\u2026"
title: "T\u1EA3i trang web"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tải xuống một trang web có nghĩa là lấy dữ liệu từ internet và lưu trữ cục bộ. Lập trình viên thực hiện điều này cho các mục đích như web scraping, phân tích ngoại tuyến, hoặc sao lưu.

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
