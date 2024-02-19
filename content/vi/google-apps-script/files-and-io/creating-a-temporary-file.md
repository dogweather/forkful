---
aliases:
- /vi/google-apps-script/creating-a-temporary-file/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:23.587696-07:00
description: "Vi\u1EC7c t\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi trong Google\
  \ Apps Script bao g\u1ED3m vi\u1EC7c sinh ra m\u1ED9t t\u1EC7p d\u1EF1 \u0111\u1ECB\
  nh cho vi\u1EC7c s\u1EED d\u1EE5ng trong ng\u1EAFn h\u1EA1n, th\u01B0\u1EDDng l\xE0\
  \ cho vi\u1EC7c x\u1EED l\xFD d\u1EEF li\u1EC7u\u2026"
lastmod: 2024-02-18 23:08:50.242288
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi trong Google Apps\
  \ Script bao g\u1ED3m vi\u1EC7c sinh ra m\u1ED9t t\u1EC7p d\u1EF1 \u0111\u1ECBnh\
  \ cho vi\u1EC7c s\u1EED d\u1EE5ng trong ng\u1EAFn h\u1EA1n, th\u01B0\u1EDDng l\xE0\
  \ cho vi\u1EC7c x\u1EED l\xFD d\u1EEF li\u1EC7u\u2026"
title: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc tạo một tệp tạm thời trong Google Apps Script bao gồm việc sinh ra một tệp dự định cho việc sử dụng trong ngắn hạn, thường là cho việc xử lý dữ liệu trung gian, gỡ lỗi, hoặc mục đích cache. Các nhà phát triển thực hiện điều này để quản lý dữ liệu một cách tạm thời mà không làm rối tung không gian lưu trữ vĩnh viễn hoặc khi tính vĩnh viễn của dữ liệu không cần thiết ngoài phạm vi của quá trình hiện tại.

## Cách thực hiện:

Trong Google Apps Script, việc tạo một tệp tạm thời có thể được thực hiện sử dụng dịch vụ DriveApp, nó cung cấp một phương pháp trực tiếp để tạo, đọc và xóa tệp trong Google Drive. Dưới đây là cách bạn có thể tạo một tệp văn bản tạm thời, ghi một số dữ liệu vào đó, sau đó remove nó sau khi sử dụng:

```javascript
function createTemporaryFile() {
  // Tạo một tệp tạm thời có tên "tempFile.txt"
  var tempFile = DriveApp.createFile('tempFile.txt', 'Nội dung tạm thời', MimeType.PLAIN_TEXT);
  
  // Log URL của tệp để truy cập hoặc gỡ lỗi
  Logger.log('Tệp tạm thời đã được tạo: ' + tempFile.getUrl());
  
  // Ví dụ thao tác: Đọc nội dung tệp
  var content = tempFile.getBlob().getDataAsString();
  Logger.log('Nội dung của tempFile: ' + content);
  
  // Giả sử quá trình hoàn thành và tệp không còn cần thiết
  // Xóa tệp tạm thời
  tempFile.setTrashed(true);
  
  // Xác nhận việc xóa
  Logger.log('Tệp tạm thời đã được xóa');
}
```

Khi chạy script này sẽ xuất ra:

```
Tệp tạm thời đã được tạo: [URL của tệp tạm thời được tạo]
Nội dung của tempFile: Nội dung tạm thời
Tệp tạm thời đã được xóa
```

Script ví dụ này trình bày việc tạo một tệp tạm thời, thực hiện một thao tác để đọc nội dung của nó và cuối cùng, remove tệp để dọn dẹp.

## Sâu hơn nữa

Khái niệm tạo tệp tạm thời trong phát triển phần mềm cũ như khái niệm quản lý tệp. Trong hệ thống tệp truyền thống, tệp tạm thời thường được tạo trong các thư mục temp được chỉ định và quan trọng cho nhiều quá trình trung gian, như sắp xếp các tập dữ liệu lớn, giữ dữ liệu phiên cho ứng dụng web, hoặc lưu trữ đoạn dữ liệu trong quá trình chuyển đổi tệp.

Trong Google Apps Script, quá trình tạo tệp tạm thời tận dụng cơ sở hạ tầng của Google Drive, đem lại sự kết hợp thú vị của quản lý tệp dựa trên đám mây với các khái niệm lập trình truyền thống. Tuy nhiên, phương pháp này để tạo tệp tạm thời trong Google Drive không phải không có hạn chế và chi phí, xem xét các giới hạn băng thông mà Google Drive áp đặt. Ngoài ra, độ trễ khi truy cập Google Drive qua mạng so với hệ thống tệp cục bộ có thể là yếu tố quan trọng đối với các ứng dụng cần hiệu suất cao.

Là các lựa chọn thay thế, nhà phát triển có thể cân nhắc sử dụng Google Sheets cho các tập dữ liệu nhỏ yêu cầu lưu trữ tạm thời trong quá trình tính toán, hoặc Google Cloud Storage cho các ứng dụng yêu cầu thao tác đọc/ghi với hiệu suất cao và dung lượng lưu trữ lớn hơn. Mỗi giải pháp này đều mang lại những sự đánh đổi khác nhau về độ trễ, giới hạn lưu trữ và sự dễ dàng sử dụng từ Google Apps Script. Cuối cùng, sự lựa chọn phụ thuộc vào yêu cầu cụ thể của ứng dụng và cơ sở hạ tầng hiện hành mà nó hoạt động trong.
