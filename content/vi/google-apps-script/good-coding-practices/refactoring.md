---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:47.193775-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Google Apps Script, m\u1ED9t t\xEC\
  nh hu\u1ED1ng ph\u1ED5 bi\u1EBFn \u0111\u01B0\u1EE3c h\u01B0\u1EDFng l\u1EE3i t\u1EEB\
  \ vi\u1EC7c t\xE1i c\u1EA5u tr\xFAc l\xE0 vi\u1EC7c \u0111\u01A1n gi\u1EA3n h\xF3\
  a c\xE1c k\u1ECBch b\u1EA3n giao ti\u1EBFp v\u1EDBi\u2026"
lastmod: '2024-03-13T22:44:36.051885-06:00'
model: gpt-4-0125-preview
summary: "Trong Google Apps Script, m\u1ED9t t\xECnh hu\u1ED1ng ph\u1ED5 bi\u1EBF\
  n \u0111\u01B0\u1EE3c h\u01B0\u1EDFng l\u1EE3i t\u1EEB vi\u1EC7c t\xE1i c\u1EA5\
  u tr\xFAc l\xE0 vi\u1EC7c \u0111\u01A1n gi\u1EA3n h\xF3a c\xE1c k\u1ECBch b\u1EA3\
  n giao ti\u1EBFp v\u1EDBi Google Sheets ho\u1EB7c Docs m\u1ED9t c\xE1ch n\u1EB7\
  ng n\u1EC1."
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3 ngu\u1ED3n"
weight: 19
---

## Cách thực hiện:
Trong Google Apps Script, một tình huống phổ biến được hưởng lợi từ việc tái cấu trúc là việc đơn giản hóa các kịch bản giao tiếp với Google Sheets hoặc Docs một cách nặng nề. Ban đầu, các kịch bản có thể được viết một cách nhanh chóng để đạt kết quả nhanh. Theo thời gian, khi kịch bản phát triển, nó trở nên cồng kềnh. Hãy cùng xem xét một ví dụ về tái cấu trúc để đạt được khả năng đọc và hiệu quả tốt hơn.

**Kịch bản Gốc:**

```javascript
function logSheetNames() {
  var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  for (var i = 0; i < sheets.length; i++) {
    Logger.log(sheets[i].getName());
  }
}
```

Hàm này ghi lại tên của mỗi bảng tính trong một Google Spreadsheet. Mặc dù nó hoạt động tốt, nhưng nó sử dụng các thực hành JavaScript lỗi thời và thiếu rõ ràng.

**Kịch bản Đã Tái Cấu Trúc:**

```javascript
function logSheetNames() {
  const sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  sheets.forEach(sheet => Logger.log(sheet.getName()));
}
```

Trong phiên bản đã tái cấu trúc, chúng tôi đã chuyển sang sử dụng `const` cho các biến không thay đổi, làm cho ý định của chúng tôi rõ ràng hơn. Chúng tôi cũng sử dụng phương thức `forEach`, một cách tiếp cận hiện đại và ngắn gọn hơn để lặp qua các mảng, tăng cường khả năng đọc.

**Kết Quả Mẫu (cho cả hai kịch bản):**

Kết quả trong Logger sẽ trông như thế này, giả sử tài liệu Google Sheets của bạn có hai bảng tên là "Expenses" và "Revenue":

```
[20-04-2023 10:00:00: INFO] Expenses
[20-04-2023 10:00:01: INFO] Revenue
```

Kịch bản đã tái cấu trúc đạt được kết quả tương tự nhưng sạch sẽ và dễ hiểu hơn ngay từ cái nhìn đầu tiên.

## Sâu hơn nữa
Tái cấu trúc trong Google Apps Script một phần kế thừa các nguyên tắc từ thực hành kỹ thuật phần mềm rộng lớn hơn. Nó trở nên được công nhận và cấu trúc hơn như một khái niệm vào cuối những năm 1990, đáng chú ý nhờ cuốn sách tiên phong "Refactoring: Improving the Design of Existing Code" (1999) của Martin Fowler, cung cấp một hướng dẫn toàn diện về các kỹ thuật tái cấu trúc. Mặc dù các chi tiết cụ thể của việc tái cấu trúc có thể khác nhau giữa các ngôn ngữ lập trình do sự khác biệt về cú pháp và chức năng, mục tiêu cốt lõi vẫn giống nhau: cải thiện mã mà không thay đổi hành vi bên ngoài của nó.

Trong bối cảnh của Google Apps Script, một khía cạnh quan trọng cần xem xét trong quá trình tái cấu trúc là các hạn ngạch và giới hạn do Google áp đặt. Mã được tái cấu trúc một cách hiệu quả không chỉ dễ đọc hơn mà còn chạy nhanh và đáng tin cậy hơn trong những ràng buộc này. Ví dụ, các hoạt động hàng loạt (`Range.setValues()` thay vì thiết lập giá trị cho từng ô một) có thể giảm đáng kể thời gian thực thi và tiêu thụ hạn ngạch.

Tuy nhiên, cần lưu ý rằng, đối với một số dự án phức tạp, Google Apps Script có thể không đáp ứng được do các hạn chế này. Trong những trường hợp như vậy, việc tìm hiểu về các lựa chọn khác như Google Cloud Functions hoặc người anh em mới hơn của Apps Script, AppSheet, có thể cung cấp quy mô và chức năng tốt hơn.

Cuối cùng, mặc dù tái cấu trúc là một kỹ năng quan trọng trong việc bảo trì và cải thiện các dự án Google Apps Script, việc hiểu rõ giới hạn của môi trường và xem xét các giải pháp thay thế cũng quan trọng không kém cho việc cung cấp mã hiệu quả, mạnh mẽ và bảo trì được.
