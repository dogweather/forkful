---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:05.307098-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Google Apps Script, c\xE1ch ch\xEDnh \u0111\
  \u1EC3 g\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP l\xE0 s\u1EED d\u1EE5ng d\u1ECBch\
  \ v\u1EE5 `UrlFetchApp`. D\u1ECBch v\u1EE5 n\xE0y cung c\u1EA5p c\xE1c ph\u01B0\u01A1\
  ng th\u1EE9c \u0111\u1EC3 th\u1EF1c\u2026"
lastmod: '2024-03-13T22:44:36.035718-06:00'
model: gpt-4-0125-preview
summary: "Trong Google Apps Script, c\xE1ch ch\xEDnh \u0111\u1EC3 g\u1EEDi m\u1ED9\
  t y\xEAu c\u1EA7u HTTP l\xE0 s\u1EED d\u1EE5ng d\u1ECBch v\u1EE5 `UrlFetchApp`."
title: "G\u1EEDi y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Làm thế nào:
Trong Google Apps Script, cách chính để gửi một yêu cầu HTTP là sử dụng dịch vụ `UrlFetchApp`. Dịch vụ này cung cấp các phương thức để thực hiện các yêu cầu HTTP GET và POST. Dưới đây là một ví dụ đơn giản về việc thực hiện một yêu cầu GET để truy xuất dữ liệu JSON:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

Đối với một yêu cầu POST, thường được sử dụng để gửi dữ liệu lên máy chủ, bạn cần đưa thêm thông tin chi tiết vào tham số tùy chọn:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // Chuyển đổi đối tượng JavaScript thành chuỗi JSON
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

Những đoạn mã này cho thấy các thực hiện cơ bản của yêu cầu GET và POST. Kết quả sẽ phụ thuộc vào phản hồi của API và có thể được xem trong Logger của Google Apps Script.

## Đào sâu
Dịch vụ `UrlFetchApp` của Google Apps Script đã phát triển đáng kể từ khi ra đời, cung cấp khả năng điều khiển tinh vi hơn đối với yêu cầu HTTP với các tính năng như thiết lập tiêu đề, payload, và xử lý multipart/form-data cho việc tải lên tệp. Mặc dù nó cung cấp một phương tiện đơn giản để tích hợp các dịch vụ web bên ngoài, các nhà phát triển đến từ các ngôn ngữ backend mạnh mẽ hơn có thể thấy chức năng của nó hạn chế một chút so với các thư viện như `requests` của Python hay `fetch` API trong Node.js của JavaScript.

Một hạn chế đáng chú ý là giới hạn thời gian thực thi cho Google Apps Script, ảnh hưởng đến các yêu cầu thực hiện trong thời gian dài. Hơn nữa, mặc dù `UrlFetchApp` bao gồm một phạm vi rộng lớn của trường hợp sử dụng, các tình huống phức tạp hơn liên quan đến xác thực OAuth hoặc xử lý tải lớn dữ liệu có thể yêu cầu các giải pháp sáng tạo hoặc tận dụng thêm nguồn lực của Google Cloud.

Tuy nhiên, đối với hầu hết các tích hợp mà nhà phát triển Google Workspace gặp phải - từ tự động hóa việc truy xuất dữ liệu đến việc đăng cập nhật lên các dịch vụ bên ngoài - `UrlFetchApp` cung cấp một công cụ mạnh mẽ, dễ tiếp cận. Việc tích hợp vào Google Apps Script có nghĩa là không cần thư viện bên ngoài hoặc thiết lập phức tạp, khiến việc thực hiện các yêu cầu HTTP trở nên tương đối dễ dàng trong các giới hạn của Google Apps Script. Khi bề mặt của các API web tiếp tục mở rộng, `UrlFetchApp` vẫn là một cây cầu quan trọng cho các chương trình Google Apps Script tương tác với thế giới bên ngoài hệ sinh thái của Google.
