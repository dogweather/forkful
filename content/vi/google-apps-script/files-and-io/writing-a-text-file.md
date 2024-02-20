---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:45.699974-07:00
description: "Vi\u1EC7c vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong Google\
  \ Apps Script gi\xFAp c\xE1c nh\xE0 ph\xE1t tri\u1EC3n c\xF3 th\u1EC3 l\u01B0u tr\u1EEF\
  \ d\u1EEF li\u1EC7u m\u1ED9t c\xE1ch b\u1EC1n v\u1EEFng, khi\u1EBFn n\xF3 c\xF3\
  \ th\u1EC3 truy c\u1EADp \u0111\u1EC3 s\u1EED d\u1EE5ng\u2026"
lastmod: 2024-02-19 22:04:55.256322
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong Google Apps\
  \ Script gi\xFAp c\xE1c nh\xE0 ph\xE1t tri\u1EC3n c\xF3 th\u1EC3 l\u01B0u tr\u1EEF\
  \ d\u1EEF li\u1EC7u m\u1ED9t c\xE1ch b\u1EC1n v\u1EEFng, khi\u1EBFn n\xF3 c\xF3\
  \ th\u1EC3 truy c\u1EADp \u0111\u1EC3 s\u1EED d\u1EE5ng\u2026"
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc viết một tệp văn bản trong Google Apps Script giúp các nhà phát triển có thể lưu trữ dữ liệu một cách bền vững, khiến nó có thể truy cập để sử dụng hoặc phân tích trong tương lai. Thao tác này là một phương pháp phổ biến để ghi lại log, lưu cấu hình, hoặc xuất thông tin trong một định dạng đơn giản, dễ đọc.

## Làm thế nào:

Việc tạo và viết vào một tệp văn bản trong Google Apps Script có thể được thực hiện thông qua dịch vụ Google DriveApp. Dưới đây là hướng dẫn từng bước với ví dụ mã để bắt đầu:

**Bước 1: Tạo một Tệp Văn bản Mới**

```javascript
// Tạo một tệp văn bản mới ở root của Google Drive
var file = DriveApp.createFile('Example.txt', 'Xin chào, thế giới!');
```

Đoạn mã này tạo ra một tệp văn bản có tên là "Example.txt" với nội dung "Xin chào, thế giới!".

**Bước 2: Mở và Viết vào một Tệp Văn bản Đã Có**

Nếu bạn cần mở một tệp đã có và viết vào nó, bạn có thể sử dụng phương thức `getFileById(id)` để truy xuất tệp đó và sau đó là xử lý nội dung của nó.

```javascript
// Lấy một tệp bằng ID của nó và thêm nội dung mới
var fileId = 'YOUR_FILE_ID_HERE'; // Thay YOUR_FILE_ID_HERE bằng ID tệp thực tế của bạn
var file = DriveApp.getFileById(fileId);
file.setContent(file.getBlob().getDataAsString() + '\nNội dung mới được thêm vào.');
```

Mã này truy xuất một tệp đã có sử dụng ID duy nhất của nó, sau đó thêm "Nội dung mới được thêm vào." vào bất kỳ nội dung nào đã có trước đó.

**Kết Quả Mẫu**

Không có đầu ra cụ thể nào được hiển thị bằng cách chạy các đoạn mã trên, nhưng nếu bạn điều hướng đến Google Drive nơi tệp được lưu trữ, bạn sẽ thấy "Example.txt" cho đoạn mã đầu tiên. Đối với đoạn mã thứ hai, nếu bạn mở tệp được chỉ định bằng ID, bạn sẽ thấy nội dung ban đầu theo sau là dòng mới "Nội dung mới được thêm vào."

## Tìm Hiểu Sâu

Việc viết một tệp văn bản trong Google Apps Script tận dụng dịch vụ DriveApp, cơ bản khai thác khả năng của Google Drive cho việc lưu trữ và quản lý tệp. Phương pháp này trở lại từ thời điểm ra đời của Google Apps Script, được thiết kế để tự động hóa các tác vụ dễ dàng qua bộ công cụ năng suất của Google, bao gồm cả Drive.

Mặc dù trực tiếp thao tác các tệp qua Google Apps Script khá dễ dàng và tích hợp chặt chẽ với Google Workspace, các nhà phát triển đến từ các lĩnh vực khác (ví dụ, Python, Node.js) có thể thấy nó khác biệt so với làm việc với hệ thống tệp cục bộ hoặc các dịch vụ lưu trữ đám mây khác như AWS S3. Những nền tảng này thường cung cấp một bộ khả năng thao tác tệp phức tạp hơn nhưng yêu cầu thiết lập bổ sung cho xác thực và quyền.

Đối với các trường hợp yêu cầu khả năng quản lý hoặc xử lý tệp nâng cao hơn ngoài tệp văn bản đơn giản (như xử lý dữ liệu nhị phân hoặc thao tác hệ thống tệp rộng lớn), các nhà phát triển có thể xem xét sử dụng dịch vụ Google Cloud Platform (ví dụ, Cloud Storage) kết hợp với Google Apps Script. Những lựa chọn khác, mặc dù mạnh mẽ hơn, cũng giới thiệu một đường cong học tập dốc hơn và có thể chi phí cao hơn, tùy thuộc vào quy mô dự án.

Kết luận, trong khi Google Apps Script cung cấp một cách tiếp cận dễ dàng và hiệu quả để quản lý tệp trong Google Drive, bao gồm việc viết tệp văn bản, thì việc hiểu rõ giới hạn của nó và khám phá các công nghệ Google khác khi cần thiết để đáp ứng các yêu cầu phức tạp hơn là quan trọng.
