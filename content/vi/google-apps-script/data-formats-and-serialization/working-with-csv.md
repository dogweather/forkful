---
title:                "Làm việc với CSV"
aliases:
- /vi/google-apps-script/working-with-csv.md
date:                  2024-02-01T22:05:24.041478-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/working-with-csv.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với các tệp CSV (Comma-Separated Values) trong Google Apps Script bao gồm việc đọc, sửa đổi và ghi các tệp văn bản thuần túy, nơi mỗi dòng đại diện cho một bản ghi dữ liệu với các giá trị được phân tách bằng dấu phẩy. Các lập trình viên thực hiện việc này để dễ dàng trao đổi dữ liệu giữa các ứng dụng khác nhau, cơ sở dữ liệu, hoặc ngôn ngữ lập trình do sự chấp nhận rộng rãi của CSV với tư cách là một định dạng trao đổi dữ liệu dựa trên văn bản đơn giản.

## Làm thế nào:

### Đọc Dữ liệu CSV

Để đọc dữ liệu CSV từ một tệp được lưu trữ trên Google Drive, đầu tiên bạn cần lấy nội dung của tệp dưới dạng chuỗi, sau đó phân tích nó. Google Apps Script làm cho việc lấy nội dung tệp trở nên dễ dàng với dịch vụ DriveApp.

```javascript
function readCSV() {
  var fileId = 'YOUR_FILE_ID_HERE'; // Thay thế bằng ID thực sự của tệp
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // Ghi nhật ký các ô của mỗi hàng
  }
}
```

### Ghi Dữ liệu CSV

Tạo và ghi vào một tệp CSV bao gồm việc xây dựng một chuỗi với các giá trị được phân cách bằng dấu phẩy và dấu xuống dòng, sau đó lưu hoặc xuất nó. Ví dụ này minh hoạ cách tạo một tệp CSV mới trên Google Drive.

```javascript
function writeCSV() {
  var folderId = 'YOUR_FOLDER_ID_HERE'; // Thay thế bằng ID của thư mục Drive nơi tệp mới sẽ được tạo
  var csvContent = "Name,Age,Occupation\nJohn Doe,29,Engineer\nJane Smith,34,Designer";
  var fileName = "example.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### Đầu ra Mẫu

Khi ghi nhật ký các ô hàng từ việc đọc một CSV:

```plaintext
[John, 29, Engineer]
[Jane, 34, Designer]
```

Khi ghi, một tệp có tên "example.csv" được tạo với nội dung:

```plaintext
Name,Age,Occupation
John Doe,29,Engineer
Jane Smith,34,Designer
```

## Sâu hơn

Truyền thống, các tệp CSV được ưa chuộng vì sự đơn giản và khả năng đọc dễ của chúng, khiến chúng dễ tiếp cận đối với những người không phải lập trình viên và hữu ích cho các tác vụ kiểm tra dữ liệu nhanh chóng. Tuy nhiên, Google Apps Script hoạt động trong lĩnh vực của hệ sinh thái Google, nơi Google Sheets đóng vai trò như một lựa chọn thay thế mạnh mẽ và thân thiện với người dùng cho việc thao tác CSV. Sheets không chỉ cung cấp một GUI cho việc chỉnh sửa dữ liệu mà còn hỗ trợ công thức phức tạp, kiểu dáng và nhiều tính năng khác mà CSV thô thiếu.

Mặc dù vậy, khả năng thao tác trực tiếp với CSV trong Google Apps Script vẫn quan trọng cho các tác vụ tự động, đặc biệt khi xử lý với các hệ thống bên ngoài sinh ra hoặc yêu cầu dữ liệu dưới dạng CSV. Chẳng hạn, tích hợp với hệ thống cũ, xuất dữ liệu để sử dụng trong các ứng dụng khác, hoặc tiền xử lý trước khi đưa dữ liệu vào Google Sheets.

Hơn nữa, khả năng làm việc với tệp CSV của Google Apps Script có thể được mở rộng với dịch vụ Utilities cho nhu cầu mã hoá nâng cao, hoặc kết nối với các API bên ngoài cho các tác vụ chuyển đổi, phân tích cú pháp hoặc xác nhận. Tuy nhiên, khi làm việc với các bộ dữ liệu lớn hoặc yêu cầu thao tác phức tạp, hãy xem xét sử dụng API của Google Sheets hoặc khám phá BigQuery để có khả năng xử lý dữ liệu mạnh mẽ hơn.

Mặc dù sự đơn giản vẫn là lý do chính khiến CSV phổ biến, những lựa chọn thay thế này cung cấp một bộ đặc tính phong phú hơn cho việc xử lý dữ liệu trong hệ sinh thái Google Cloud rộng lớn.
