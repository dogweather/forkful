---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:01.552507-07:00
description: "Vi\u1EC7c l\xE0m vi\u1EC7c v\u1EDBi XML trong Google Apps Script cho\
  \ ph\xE9p l\u1EADp tr\xECnh vi\xEAn ph\xE2n t\xEDch c\xFA ph\xE1p, ch\u1EC9nh s\u1EED\
  a, v\xE0 t\u1EA1o d\u1EEF li\u1EC7u XML, \u0111\xE2y l\xE0 \u0111i\u1EC1u c\u1EA7\
  n thi\u1EBFt cho c\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.073217-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c l\xE0m vi\u1EC7c v\u1EDBi XML trong Google Apps Script cho ph\xE9\
  p l\u1EADp tr\xECnh vi\xEAn ph\xE2n t\xEDch c\xFA ph\xE1p, ch\u1EC9nh s\u1EEDa,\
  \ v\xE0 t\u1EA1o d\u1EEF li\u1EC7u XML, \u0111\xE2y l\xE0 \u0111i\u1EC1u c\u1EA7\
  n thi\u1EBFt cho c\xE1c d\u1ECBch v\u1EE5 web v\xE0 c\u1EA5u h\xECnh."
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

## Gì & Tại Sao?

Việc làm việc với XML trong Google Apps Script cho phép lập trình viên phân tích cú pháp, chỉnh sửa, và tạo dữ liệu XML, đây là điều cần thiết cho các dịch vụ web và cấu hình. Lập trình viên áp dụng cách tiếp cận này để tích hợp với các hệ thống cũ, thực hiện web scraping, hoặc liên lạc với nhiều API vẫn dựa vào XML hơn là JSON cho trao đổi dữ liệu.

## Làm Thế Nào:

Google Apps Script cung cấp `XmlService` để làm việc với dữ liệu XML. Dưới đây chúng tôi trình bày cách phân tích cú pháp một chuỗi XML, chỉnh sửa nội dung của nó, và tạo một chuỗi XML mới.

Phân tích cú pháp một chuỗi XML:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Logs: Hello
}
```

Để chỉnh sửa XML, bạn có thể muốn thêm một phần tử con mới:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Ghi nhận chuỗi XML mới với phần tử con được thêm vào
}
```

Tạo chuỗi XML từ đầu:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Kết quả: <root><child>Hello World</child></root>
}
```

## Sâu Hơn

Lịch sử, XML (Extensible Markup Language) là chuẩn mặc định cho trao đổi dữ liệu trước khi JSON xuất hiện như một lựa chọn nhẹ nhàng hơn. Cú pháp chi tiết và mô hình phân tích cú pháp nghiêm ngặt của XML cung cấp một định dạng dữ liệu chắc chắn, mặc dù nặng nề. Trong Google Apps Script, API `XmlService` bao gồm việc tạo, phân tích cú pháp, và chỉnh sửa dữ liệu XML, thừa nhận tầm quan trọng liên tục của nó trong các hệ thống cũ và doanh nghiệp, dịch vụ web SOAP, và các tệp cấu hình cho ứng dụng.

Mặc dù JSON hiện diện rộng rãi trong phát triển web hiện đại vì sự đơn giản và dễ sử dụng với JavaScript, XML vẫn có liên quan trong các khu vực mà kiểm tra tài liệu và cấu trúc phân cấp là rất quan trọng. Tuy nhiên, đối với các dự án mới, đặc biệt là những dự án nghiêng về web APIs, JSON thường là lựa chọn thực tiễn hơn do bản chất nhẹ dàng và tích hợp mượt mà với JavaScript.

Việc hiểu XML và cách xử lý nó trong Google Apps Script là điều cốt yếu cho các nhà phát triển làm việc trong môi trường cần tích hợp với các hệ thống cũ hoặc API doanh nghiệp cụ thể. Tuy nhiên, khi bắt đầu các dự án mới hoặc khi linh hoạt là chìa khóa, cân nhắc nhu cầu về XML so với các lựa chọn khác như JSON là điều khuyến khích.
