---
title:                "Tìm kiếm và thay thế văn bản"
date:                  2024-02-01T22:01:51.211448-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/google-apps-script/searching-and-replacing-text.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tìm kiếm và thay thế văn bản trong Google Apps Script là việc xác định chương trình các chuỗi cụ thể trong một tài liệu, bảng tính, hoặc bất kỳ loại nội dung Google Apps nào khác, và thay thế chúng bằng các giá trị văn bản khác. Lập trình viên sử dụng tính năng này để tự động hóa việc chỉnh sửa khối lượng lớn nội dung, sửa lỗi phổ biến, chuẩn hóa thuật ngữ trên các tài liệu, hoặc chèn dữ liệu động vào các mẫu.

## Làm thế nào:

Google Apps Script cung cấp một cách đơn giản để tìm và thay thế văn bản, đặc biệt là trong Google Docs và Sheets. Dưới đây là các ví dụ cho cả hai.

### Google Docs:

Để tìm và thay thế văn bản trong một Tài liệu Google, bạn chủ yếu tương tác với lớp `DocumentApp`.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // Để tìm và thay thế một cụm từ cụ thể
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// Cách sử dụng
searchReplaceInDoc();
```

Đoạn mã này tìm kiếm tất cả các trường hợp của `'searchText'` trong Tài liệu Google đang hoạt động và thay thế chúng bởi `'replacementText'`.

### Google Sheets:

Tương tự, trong Google Sheets, bạn có thể sử dụng `SpreadsheetApp` để thực hiện các thao tác tìm và thay thế:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // Tìm và thay thế trong trang tính đang hoạt động hiện tại
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// Cách sử dụng
searchReplaceInSheet();
```

Trong ví dụ này, `createTextFinder('searchText')` tìm kiếm 'searchText' trong trang tính đang hoạt động, và `replaceAllWith('replacementText')` thay thế tất cả các trường hợp bởi 'replacementText'.

## Sâu hơn

Tính năng tìm và thay thế trong Google Apps Script chịu ảnh hưởng nặng nề từ bản chất dựa trên web của nó, cho phép các script thao tác văn bản một cách liền mạch trên các Google Apps khác nhau. Trên thực tế, khả năng này xuất phát từ bối cảnh rộng lớn hơn về xử lý và thao tác văn bản trong lập trình, nơi mà biểu thức chính quy và các hàm chuỗi trong ngôn ngữ như Perl và Python đặt ra một tiêu chuẩn cao về sự linh hoạt và mạnh mẽ.

Mặc dù tính năng tìm và thay thế của Google Apps Script mạnh mẽ đối với các thay thế đơn giản, nhưng nó thiếu khả năng sử dụng biểu thức chính quy đầy đủ như một số ngôn ngữ khác. Ví dụ, trong khi bạn có thể sử dụng biểu thức chính quy cơ bản trong `createTextFinder` trong Google Sheets, các tùy chọn cho việc so khớp và thao tác mô hình phức tạp hạn chế so với Perl hoặc Python.

Đối với các nhu cầu xử lý văn bản tiên tiến hơn, lập trình viên có thể chuyển đổi nội dung Google Docs hoặc Sheets ra định dạng có thể được xử lý bên ngoài bằng các ngôn ngữ mạnh mẽ hơn hoặc sử dụng Google Apps Script để gọi các API hoặc dịch vụ bên ngoài cung cấp khả năng thao tác văn bản tinh vi hơn.

Mặc dù vậy, đối với hầu hết các tác vụ tìm kiếm và thay thế tiêu biểu trong hệ sinh thái Google Apps, Google Apps Script cung cấp một giải pháp đơn giản, hiệu quả và dễ tích hợp, được thiết kế riêng cho nhu cầu tự động hóa và viết kịch bản trong bộ công cụ năng suất của Google.
