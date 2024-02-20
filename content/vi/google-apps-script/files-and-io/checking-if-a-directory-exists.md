---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:28.478214-07:00
description: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3\
  n t\u1EA1i trong Google Apps Script \u0111\u1EC1 c\u1EADp \u0111\u1EBFn vi\u1EC7\
  c x\xE1c minh s\u1EF1 hi\u1EC7n di\u1EC7n c\u1EE7a m\u1ED9t th\u01B0 m\u1EE5c b\xEA\
  n trong Google Drive. C\xE1c l\u1EADp\u2026"
lastmod: 2024-02-19 22:04:55.250632
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1\
  i trong Google Apps Script \u0111\u1EC1 c\u1EADp \u0111\u1EBFn vi\u1EC7c x\xE1c\
  \ minh s\u1EF1 hi\u1EC7n di\u1EC7n c\u1EE7a m\u1ED9t th\u01B0 m\u1EE5c b\xEAn trong\
  \ Google Drive. C\xE1c l\u1EADp\u2026"
title: "Ki\u1EC3m tra n\u1EBFu m\u1ED9t th\u01B0 m\u1EE5c t\u1ED3n t\u1EA1i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc kiểm tra xem một thư mục có tồn tại trong Google Apps Script đề cập đến việc xác minh sự hiện diện của một thư mục bên trong Google Drive. Các lập trình viên thường thực hiện kiểm tra này để tránh lỗi hoặc tạo thư mục trùng lặp khi quản lý tập tin và thư mục một cách lập trình.

## Cách thức:

Google Apps Script không cung cấp trực tiếp phương thức "exists" cho các thư mục. Thay vào đó, chúng ta sử dụng khả năng tìm kiếm của Google Drive để kiểm tra xem có thư mục có tên cụ thể tồn tại hay không. Dưới đây là một ví dụ từng bước:

```javascript
// Hàm kiểm tra xem thư mục có tồn tại không
function checkIfDirectoryExists(directoryName) {
  // Lấy bộ sưu tập các thư mục khớp với tên đã chỉ định
  var folders = DriveApp.getFoldersByName(directoryName);
  
  // Kiểm tra nếu có ít nhất một thư mục có tên đã chỉ định tồn tại
  if (folders.hasNext()) {
    Logger.log('Thư mục tồn tại.');
    return true;
  } else {
    Logger.log('Thư mục không tồn tại.');
    return false;
  }
}

// Ví dụ sử dụng
var directoryName = 'Thư Mục Mẫu Của Tôi';
checkIfDirectoryExists(directoryName);
```

Đầu ra mẫu:
```
Thư mục tồn tại.
```
hoặc 
```
Thư mục không tồn tại.
```

Script này tận dụng phương thức `getFoldersByName` để lấy tất cả các thư mục trong Drive của người dùng mà khớp với tên đã chỉ định. Do tên không độc nhất trong Drive, phương thức này trả về `FolderIterator`. Sự hiện diện của một mục tiếp theo (`hasNext()`) trong người lặp này chứng tỏ thư mục tồn tại.

## Diving sâu hơn

Trong lịch sử, quản lý tập tin trong môi trường web và đám mây đã phát triển đáng kể. Google Apps Script, cung cấp một API rộng lớn cho Google Drive, cho phép thực hiện các thao tác quản lý tập tin và thư mục tinh vi, bao gồm cả cơ chế tìm kiếm và kiểm tra đã được trình bày. Tuy nhiên, một điểm đáng chú ý là sự thiếu vắng của một kiểm tra tồn tại trực tiếp, có thể do Google Drive cho phép nhiều thư mục cùng tên, điều này trái ngược với nhiều hệ thống tập tin yêu cầu tên duy nhất trong cùng một thư mục.

Trong bối cảnh này, việc sử dụng phương thức `getFoldersByName` là một giải pháp tạm thời hiệu quả nhưng có thể giới thiệu sự không hiệu quả trong tình huống có số lượng lớn thư mục có tên trùng lặp. Một cách tiếp cận thay thế có thể bao gồm việc duy trì một chỉ mục hoặc quy ước đặt tên cụ thể cho ứng dụng để đảm bảo kiểm tra nhanh chóng, đặc biệt khi hiệu suất trở thành mối quan tâm quan trọng.

Mặc dù cách tiếp cận của Google Apps Script có vẻ ít trực tiếp hơn so với các kiểm tra tồn tại tập tin trong các ngôn ngữ lập trình giao tiếp trực tiếp với một hệ thống tập tin đơn nhất, nó phản ánh sự cần thiết phải xử lý các phức tạp của lưu trữ tập tin dựa trên đám mây. Các nhà phát triển sử dụng Google Apps Script để quản lý Drive nên xem xét những nét tinh tế này, tối ưu hóa cho điểm mạnh và giới hạn của Google Drive.
