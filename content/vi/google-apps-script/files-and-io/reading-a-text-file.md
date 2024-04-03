---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:26.887898-07:00
description: "Vi\u1EC7c \u0111\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong Google\
  \ Apps Script (GAS) bao g\u1ED3m vi\u1EC7c truy c\u1EADp v\xE0 tr\xEDch xu\u1EA5\
  t d\u1EEF li\u1EC7u v\u0103n b\u1EA3n t\u1EEB c\xE1c t\u1EC7p \u0111\u01B0\u1EE3\
  c l\u01B0u tr\u1EEF tr\xEAn Google Drive ho\u1EB7c\u2026"
lastmod: '2024-03-13T22:44:36.064093-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c \u0111\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong Google\
  \ Apps Script (GAS) bao g\u1ED3m vi\u1EC7c truy c\u1EADp v\xE0 tr\xEDch xu\u1EA5\
  t d\u1EEF li\u1EC7u v\u0103n b\u1EA3n t\u1EEB c\xE1c t\u1EC7p \u0111\u01B0\u1EE3\
  c l\u01B0u tr\u1EEF tr\xEAn Google Drive ho\u1EB7c c\xE1c d\u1ECBch v\u1EE5 l\u01B0\
  u tr\u1EEF \u0111\xE1m m\xE2y kh\xE1c c\xF3 th\u1EC3 truy c\u1EADp \u0111\u01B0\u1EE3\
  c."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Làm thế nào:
Để bắt đầu đọc một tệp văn bản với Google Apps Script, bạn thường cần sử dụng Google Drive API. Dưới đây là một ví dụ cơ bản minh họa cách đọc một tệp từ Google Drive:

```javascript
function readFileContents(fileId) {
  // Lấy tệp Google Drive theo ID
  var file = DriveApp.getFileById(fileId);
  
  // Lấy dữ liệu blob dưới dạng văn bản
  var text = file.getBlob().getDataAsString();
  
  // Ghi nội dung vào nhật ký Google Apps Script
  Logger.log(text);
  return text;
}
```

*Mẫu đầu ra trong nhật ký:*

```
Hello, world! This is a test text file.
```

Trong ví dụ này, `fileId` là định danh duy nhất của tệp mà bạn muốn đọc. Dịch vụ `DriveApp` tìm nạp tệp và `getDataAsString()` đọc nội dung của nó dưới dạng một chuỗi. Sau đó, bạn có thể thao tác hoặc sử dụng văn bản này như mong muốn.

## Sâu hơn
Trong quá khứ, việc đọc các tệp văn bản trong các ứng dụng dựa trên web, như những ứng dụng được xây dựng với Google Apps Script, gặp phải thách thức do các hạn chế bảo mật của trình duyệt và bản chất không đồng bộ của JavaScript. Google Apps Script đã làm đơn giản hoá việc này với các dịch vụ trừu tượng như `DriveApp`, cung cấp một API cấp cao để tương tác với các tệp trong Google Drive.

Tuy nhiên, một khía cạnh quan trọng cần xem xét là hiệu suất và giới hạn thời gian thực thi được Google Apps Script áp đặt, đặc biệt khi đọc các tệp lớn hoặc thực hiện các thao tác phức tạp với dữ liệu. Trong một số trường hợp, việc sử dụng trực tiếp các dịch vụ Google Cloud từ một hệ thống backend mạnh mẽ hơn hoặc tiền xử lý các tệp thành các phần dễ quản lý hơn có thể hiệu quả hơn.

Đối với việc xử lý tệp phức tạp hoặc khi hiệu suất thời gian thực là quan trọng, các lựa chọn khác như Google Cloud Functions, hỗ trợ Node.js, Python và Go, có thể cung cấp nhiều sự linh hoạt và nguồn lực tính toán hơn. Dù vậy, cho các nhiệm vụ đơn giản trong hệ sinh thái Google, đặc biệt là khi sự đơn giản và tích hợp dễ dàng với các sản phẩm Google là tiêu chí hàng đầu, Google Apps Script cung cấp một phương thức tiếp cận cực kỳ thân thiện với người dùng.
