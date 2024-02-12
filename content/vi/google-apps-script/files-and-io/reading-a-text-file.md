---
title:                "Đọc một tệp văn bản"
aliases:
- /vi/google-apps-script/reading-a-text-file.md
date:                  2024-02-01T22:00:26.887898-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc một tệp văn bản"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/reading-a-text-file.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc đọc một tệp văn bản trong Google Apps Script (GAS) bao gồm việc truy cập và trích xuất dữ liệu văn bản từ các tệp được lưu trữ trên Google Drive hoặc các dịch vụ lưu trữ đám mây khác có thể truy cập được. Các lập trình viên thường cần đọc những tệp này để nhập, điều chỉnh hoặc phân tích dữ liệu văn bản trực tiếp trong các dự án GAS của họ, cho phép tự động hóa và tích hợp với bộ sản phẩm của Google.

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
