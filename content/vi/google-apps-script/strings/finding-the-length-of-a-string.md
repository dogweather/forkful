---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:25.972053-07:00
description: "Vi\u1EC7c t\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i trong\
  \ Google Apps Script, m\u1ED9t ng\xF4n ng\u1EEF l\u1EADp tr\xECnh JavaScript d\u1EF1\
  a tr\xEAn \u0111\xE1m m\xE2y cho ph\xE9p b\u1EA1n t\u1EF1 \u0111\u1ED9ng h\xF3a\
  \ c\xE1c t\xE1c v\u1EE5 tr\xEAn c\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.027346-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i trong Google\
  \ Apps Script, m\u1ED9t ng\xF4n ng\u1EEF l\u1EADp tr\xECnh JavaScript d\u1EF1a tr\xEA\
  n \u0111\xE1m m\xE2y cho ph\xE9p b\u1EA1n t\u1EF1 \u0111\u1ED9ng h\xF3a c\xE1c t\xE1\
  c v\u1EE5 tr\xEAn c\xE1c\u2026"
title: "T\xECm \u0111\u1ED9 d\xE0i c\u1EE7a chu\u1ED7i"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Việc tìm chiều dài của một chuỗi trong Google Apps Script, một ngôn ngữ lập trình JavaScript dựa trên đám mây cho phép bạn tự động hóa các tác vụ trên các sản phẩm của Google, là về việc xác định số lượng ký tự trong một chuỗi. Các lập trình viên thường xuyên thực hiện thao tác này để xác minh đầu vào, lặp qua các ký tự hoặc thao tác với chuỗi cho các tác vụ tự động hóa khác nhau trong Google Apps.

## Làm thế nào:
Trong Google Apps Script, bạn có thể tìm chiều dài của một chuỗi sử dụng thuộc tính `.length`, tương tự như JavaScript. Thuộc tính này trả về số lượng ký tự trong chuỗi, bao gồm khoảng trắng và ký tự đặc biệt. Dưới đây là một số ví dụ:

```javascript
// Định nghĩa một chuỗi
var text = "Hello, World!";
// Tìm chiều dài của chuỗi
var length = text.length;
// Ghi nhật ký chiều dài
Logger.log(length); // Đầu ra: 13
```

Trong trường hợp bạn đang làm việc với đầu vào từ người dùng từ Google Forms hoặc Sheets, việc tìm chiều dài của chuỗi giúp trong việc xác thực dữ liệu:

```javascript
// Mẫu đầu vào chuỗi từ người dùng trên Google Sheets
var userEntry = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet().getRange("A1").getValue();
// Tính và ghi nhật ký chiều dài của đầu vào
Logger.log(userEntry.length); // Đầu ra phụ thuộc vào nội dung của ô A1
```

Hãy thêm một ví dụ thực tiễn bao gồm một điều kiện. Nếu đầu vào vượt quá một độ dài nhất định, bạn có thể muốn tạo ra một lỗi hoặc cảnh báo:

```javascript
var comment = "Đây là một bình luận mẫu quá dài cho cơ sở dữ liệu của chúng tôi.";
if(comment.length > 50) {
  Logger.log("Lỗi: Bình luận của bạn không nên vượt quá 50 ký tự.");
} else {
  Logger.log("Cảm ơn bạn đã gửi.");
}
// Đầu ra: Lỗi: Bình luận của bạn không nên vượt quá 50 ký tự.
```

## Sâu hơn
Trong bối cảnh của Google Apps Script, dựa trên JavaScript, thuộc tính `.length` đến từ tiêu chuẩn ECMAScript, điều chỉnh các quy định của JavaScript. Thuộc tính `.length` đã là một phần của JavaScript từ những giai đoạn đầu, cung cấp một cách đơn giản để đánh giá kích thước của một chuỗi.

Một chi tiết đáng chú ý là Google Apps Script được thực hiện trên các máy chủ của Google, không phải trong trình duyệt. Điều này có nghĩa là khi bạn đang xử lý các chuỗi và chiều dài của chúng, đặc biệt là trong các tập dữ liệu lớn được truy xuất từ Google Sheets hoặc Docs, thời gian thực hiện có thể bị ảnh hưởng do độ trễ mạng và giới hạn thời gian chạy của script.

Mặc dù `.length` là một phương pháp đơn giản và được sử dụng rộng rãi để tìm chiều dài của một chuỗi, các chiến lược thay thế có thể liên quan đến regex hoặc lặp qua một chuỗi để đếm ký tự, đặc biệt là khi xử lý các ký tự nhiều byte hoặc khi bạn cần lọc ra một số loại ký tự nhất định. Tuy nhiên, cho hầu hết các mục đích thực tiễn trong Google Apps Script, `.length` cung cấp một cách đáng tin cậy và hiệu quả để xác định chiều dài của chuỗi.

Luôn nhớ, đặc biệt là trong Google Apps Script, để xem xét ngữ cảnh mà bạn chạy code của mình. Hiệu suất và giới hạn thực thi có thể hướng dẫn bạn tối ưu hóa các thủ tục xử lý chuỗi của mình, bao gồm cách bạn xác định chiều dài của chúng.
