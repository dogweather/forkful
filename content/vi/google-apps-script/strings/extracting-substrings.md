---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:42.165591-07:00
description: "L\xE0m sao: Trong Google Apps Script, d\u1EF1a tr\xEAn JavaScript hi\u1EC7\
  n \u0111\u1EA1i, vi\u1EC7c tr\xEDch xu\u1EA5t chu\u1ED7i con c\xF3 th\u1EC3 \u0111\
  \u1EA1t \u0111\u01B0\u1EE3c th\xF4ng qua v\xE0i ph\u01B0\u01A1ng th\u1EE9c, bao\
  \ g\u1ED3m\u2026"
lastmod: '2024-03-13T22:44:36.024732-06:00'
model: gpt-4-0125-preview
summary: "Trong Google Apps Script, d\u1EF1a tr\xEAn JavaScript hi\u1EC7n \u0111\u1EA1\
  i, vi\u1EC7c tr\xEDch xu\u1EA5t chu\u1ED7i con c\xF3 th\u1EC3 \u0111\u1EA1t \u0111\
  \u01B0\u1EE3c th\xF4ng qua v\xE0i ph\u01B0\u01A1ng th\u1EE9c, bao g\u1ED3m `substring()`,\
  \ `substr()`, v\xE0 `slice()`."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Làm sao:
Trong Google Apps Script, dựa trên JavaScript hiện đại, việc trích xuất chuỗi con có thể đạt được thông qua vài phương thức, bao gồm `substring()`, `substr()`, và `slice()`. Mỗi phương thức đều có những nét đặc thù, nhưng tất cả đều phục vụ mục đích lấy các ký tự đã chỉ định từ một chuỗi.

```javascript
// Ví dụ sử dụng substring()
var str = "Hello, world!";
var result = str.substring(0, 5);
console.log(result); // Đầu ra: Hello

// Ví dụ sử dụng substr()
var resultSubstr = str.substr(7, 5);
console.log(resultSubstr); // Đầu ra: world

// Ví dụ sử dụng slice()
var resultSlice = str.slice(-6);
console.log(resultSlice); // Đầu ra: world!
```

Mỗi phương thức đều nhận hai đối số: vị trí bắt đầu và, trừ `slice()` có thể chấp nhận chỉ số âm để bắt đầu từ phía sau, vị trí kết thúc hoặc số ký tự cần trích xuất. Đáng chú ý là chuỗi gốc vẫn không thay đổi sau những thao tác này, vì chúng trả về các giá trị chuỗi mới.

## Đi sâu vào vấn đề
Trong lịch sử, các phương thức JavaScript để trích xuất các chuỗi con đã là nguồn cơn nhầm lẫn do tên gọi và chức năng tương tự nhau. Tuy nhiên, trong Google Apps Script và JavaScript hiện đại, `substring()` và `slice()` thường được sử dụng nhiều nhất, với `substr()` được coi là lỗi thời. Điều này quan trọng cần lưu ý cho những ai viết mã lập trình với tư duy hướng tới tương lai.

Sự khác biệt chính giữa `substring()` và `slice()` là cách chúng xử lý chỉ số âm; `substring()` coi chỉ số âm là 0, trong khi `slice()` có thể chấp nhận chỉ số âm để bắt đầu việc trích xuất từ cuối chuỗi. Điều này làm cho `slice()` đặc biệt hữu ích trong các trường hợp có thể không biết chính xác độ dài của chuỗi hoặc khi cần trích xuất từ cuối.

Khi quyết định phương thức nào để sử dụng cho việc trích xuất chuỗi con, sự lựa chọn thường phụ thuộc vào yêu cầu cụ thể của thao tác (ví dụ, liệu việc xử lý chỉ số âm có lợi không) và tiêu chuẩn mã hóa cá nhân hoặc nhóm. Mặc dù không có quy tắc nào phù hợp với mọi trường hợp, việc hiểu được những khác biệt tinh tế và hậu quả về hiệu năng có thể giúp đưa ra quyết định thông báo.
