---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:08.040374-07:00
description: "Vi\u1EC7c t\u1EA3i m\u1ED9t trang web b\u1EB1ng Google Apps Script bao\
  \ g\u1ED3m vi\u1EC7c l\u1EA5y n\u1ED9i dung c\u1EE7a trang web th\xF4ng qua HTML\
  \ cho c\xE1c m\u1EE5c \u0111\xEDch kh\xE1c nhau, nh\u01B0 l\u1EA5y d\u1EEF li\u1EC7\
  u web,\u2026"
lastmod: '2024-03-13T22:44:36.038409-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA3i m\u1ED9t trang web b\u1EB1ng Google Apps Script bao g\u1ED3\
  m vi\u1EC7c l\u1EA5y n\u1ED9i dung c\u1EE7a trang web th\xF4ng qua HTML cho c\xE1\
  c m\u1EE5c \u0111\xEDch kh\xE1c nhau, nh\u01B0 l\u1EA5y d\u1EEF li\u1EC7u web,\u2026"
title: "T\u1EA3i v\u1EC1 m\u1ED9t trang web"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tải một trang web bằng Google Apps Script bao gồm việc lấy nội dung của trang web thông qua HTML cho các mục đích khác nhau, như lấy dữ liệu web, trích xuất dữ liệu, hoặc theo dõi sự thay đổi. Các lập trình viên chọn thao tác này để tự động hóa việc thu thập dữ liệu hoặc công việc tích hợp, giảm thiểu công sức thủ công và đảm bảo xử lý dữ liệu theo thời gian thực.

## Cách thực hiện:

Trong Google Apps Script, dịch vụ `UrlFetchApp` là yếu tố then chốt để tải nội dung web. Dưới đây là hướng dẫn từng bước và một ví dụ đơn giản minh họa cách lấy và ghi nhật ký nội dung HTML của một trang web:

1. **Thao tác lấy dữ liệu cơ bản:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- Đoạn mã này lấy nội dung HTML của example.com và ghi nhật ký. Đây là minh chứng đơn giản về việc lấy nguồn của một trang web mà không cần bất kỳ thông số bổ sung nào.

2. **Xử lý Chuyển hướng và HTTPS:**

Đối với HTTPS hoặc xử lý chuyển hướng, mã code vẫn gần như không thay đổi, nhưng cần xem xét triển khai xử lý lỗi hoặc tùy chọn cụ thể cho việc chuyển hướng:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // Tự động theo dõi chuyển hướng
    'muteHttpExceptions': true // Tắt âm thanh các ngoại lệ có thể xảy ra để xử lý một cách nhẹ nhàng
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **Giới hạn Tốc độ và Bảng Quy định:**

Hãy lưu ý đến bảng quy định của Google Apps Script; việc sử dụng nặng có thể đòi hỏi xử lý lỗi cho giới hạn tốc độ.

## Tìm hiểu Sâu

Lịch sử, việc tải và thao tác nội dung web bắt đầu với các yêu cầu HTTP đơn giản, phát triển đáng kể với sự xuất hiện của các ngôn ngữ kịch bản. Google Apps Script cho phép thực hiện như vậy một cách dễ dàng trong hệ sinh thái G Suite, tận dụng cơ sở hạ tầng vững chắc của Google. Dịch vụ `UrlFetchApp` là một phần cốt lõi của chức năng này, đóng gói các yêu cầu HTTP/S phức tạp vào một giao diện cấp ứng dụng đơn giản.

Mặc dù rất tiện lợi, Google Apps Script có thể không phải luôn là công cụ tốt nhất cho việc lấy dữ liệu web nặng hoặc khi yêu cầu xử lý phức tạp sau khi thu thập dữ liệu do giới hạn thời gian thực thi và bảng quy định do Google đặt ra. Trong những trường hợp như vậy, các khuôn khổ lấy dữ liệu web chuyên dụng hoặc các ngôn ngữ được thiết kế cho các thao tác I/O bất đồng bộ, như Node.js với các thư viện như Puppeteer hoặc Cheerio, có thể cung cấp nhiều tính linh hoạt và sức mạnh hơn.

Hơn nữa, mặc dù Google Apps Script là một công cụ tuyệt vời để tích hợp với các Dịch vụ Google (như Sheets, Docs và Drive) và thực hiện các thao tác lấy dữ liệu nhẹ, quan trọng là phải lưu ý đến những hạn chế của môi trường thực thi của nó. Đối với các nhiệm vụ nặng, hãy xem xét sử dụng Google Cloud Functions hoặc các dịch vụ cao cấp của Apps Script với các nguồn lực tính toán bên ngoài để xử lý.
