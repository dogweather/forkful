---
title:                "Gửi yêu cầu HTTP"
aliases: - /vi/google-apps-script/sending-an-http-request.md
date:                  2024-02-01T22:02:05.307098-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi yêu cầu HTTP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/sending-an-http-request.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Gửi một yêu cầu HTTP trong Google Apps Script là việc lập trình gọi đến một máy chủ web hoặc API bên ngoài. Lập trình viên làm điều này để truy xuất hoặc gửi dữ liệu đến các dịch vụ web, tích hợp một lĩnh vực rộng lớn của nguồn và chức năng web trực tiếp vào dự án Google Apps Script của họ.

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
