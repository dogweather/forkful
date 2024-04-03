---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:10.372622-07:00
description: "T\u1ED5 ch\u1EE9c m\xE3 th\xE0nh c\xE1c h\xE0m l\xE0 v\u1EC1 vi\u1EC7\
  c c\u1EA5u tr\xFAc m\xE3 Google Apps Script c\u1EE7a b\u1EA1n b\u1EB1ng c\xE1ch\
  \ t\xE1ch c\xE1c ph\xE2n \u0111o\u1EA1n logic th\xE0nh c\xE1c kh\u1ED1i ri\xEAng\
  \ bi\u1EC7t, m\u1ED7i kh\u1ED1i th\u1EF1c\u2026"
lastmod: '2024-03-13T22:44:36.047824-06:00'
model: gpt-4-0125-preview
summary: "T\u1ED5 ch\u1EE9c m\xE3 th\xE0nh c\xE1c h\xE0m l\xE0 v\u1EC1 vi\u1EC7c c\u1EA5\
  u tr\xFAc m\xE3 Google Apps Script c\u1EE7a b\u1EA1n b\u1EB1ng c\xE1ch t\xE1ch c\xE1\
  c ph\xE2n \u0111o\u1EA1n logic th\xE0nh c\xE1c kh\u1ED1i ri\xEAng bi\u1EC7t, m\u1ED7\
  i kh\u1ED1i th\u1EF1c hi\u1EC7n m\u1ED9t nhi\u1EC7m v\u1EE5 c\u1EE5 th\u1EC3."
title: "S\u1EAFp x\u1EBFp m\xE3 l\u1EADp tr\xECnh v\xE0o h\xE0m s\u1ED1"
weight: 18
---

## Làm thế nào:
Trong Google Apps Script, dựa trên JavaScript, bạn định nghĩa hàm sử dụng từ khóa `function`, theo sau là tên hàm duy nhất, cặp dấu ngoặc đơn `()` có thể chứa tham số, và cặp dấu ngoặc nhọn `{}` bao bọc khối mã của hàm. Dưới đây là một ví dụ cơ bản:

```javascript
function greetUser() {
  var user = Session.getActiveUser().getEmail();
  Logger.log('Xin chào, ' + user + '!');
}

greetUser();
```

Kết quả mẫu:

```
Xin chào, someone@example.com!
```

Bây giờ, hãy xem xét một ví dụ thực tế hơn liên quan đến Google Sheets, nơi chúng ta tách chức năng thành hai hàm: một cho việc thiết lập bảng và một khác để điền dữ liệu vào bảng.

```javascript
function setupSheet() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = ss.getSheets()[0];
  sheet.setName('Dữ liệu Bán Hàng');
  sheet.appendRow(['Sản phẩm', 'Số lượng', 'Giá']);
}

function populateSheet(data) {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName('Dữ liệu Bán Hàng');
  data.forEach(function(row) {
    sheet.appendRow(row);
  });
}

// Khởi tạo mảng dữ liệu
var salesData = [
  ['Widgets', 15, 2.5],
  ['Gadgets', 8, 3.75]
];

// Chạy các hàm
setupSheet();
populateSheet(salesData);
```

Trong ví dụ này, `setupSheet` chuẩn bị bảng và `populateSheet` sử dụng một mảng dữ liệu bán hàng để điền vào bảng. Tách những vấn đề này làm cho mã sạch sẽ hơn và linh hoạt hơn với sự thay đổi.

## Sâu hơn
Khái niệm về việc chia mã thành các hàm không phải là mới hoặc độc đáo với Google Apps Script; đó là một thực hành lập trình cơ bản được ủng hộ trong hầu như tất cả ngôn ngữ lập trình. Lịch sử, hàm phát triển từ khái niệm toán học về việc ánh xạ đầu vào thành đầu ra, trở thành một trụ cột trong lập trình có cấu trúc. Phương pháp này thúc đẩy tính modular và tái sử dụng mã, mang lại các con đường rõ ràng cho việc kiểm tra từng phần của kịch bản.

Google Apps Script, dựa trên JavaScript, hưởng lợi đáng kể từ các hàm hàng đầu của JavaScript, cho phép các hàm được truyền dưới dạng đối số, trả về từ các hàm khác và được gán cho các biến. Tính năng này mở ra các mô hình tiên tiến như callback và lập trình chức năng, mặc dù những mô hình này có thể giới thiệu độ phức tạp có thể không cần thiết cho các tác vụ tự động hóa đơn giản trong Google Apps Script.

Đối với các dự án lớn hơn hoặc các ứng dụng phức tạp hơn, các nhà phát triển có thể khám phá việc sử dụng các tính năng mới hơn của JavaScript như các hàm mũi tên, async/await cho các hoạt động bất đồng bộ và thậm chí TypeScript cho kiểu định kiểu tĩnh. Cụ thể, TypeScript poss được biên dịch để chạy như Google Apps Script, cung cấp một con đường cho các nhà phát triển tìm kiếm kiểm tra kiểu chặt chẽ hơn và các tính năng hướng đối tượng nâng cao.

Tuy nhiên, cho hầu hết các nhu cầu kịch bản trong Google Apps suite, việc gắn bó với các hàm đơn giản, được tổ chức tốt như được minh họa cung cấp một nền tảng vững chắc. Luôn là một sự cân nhắc giữa việc tận dụng các tính năng tiên tiến cho hiệu quả và duy trì sự đơn giản cho dễ dàng bảo trì và dễ đọc.
