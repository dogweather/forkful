---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:04.459691-07:00
description: "L\xE0m th\u1EBF n\xE0o: Google Apps Script, d\u1EF1a tr\xEAn JavaScript,\
  \ cung c\u1EA5p c\xE1c ph\u01B0\u01A1ng ph\xE1p th\u1EB3ng th\u1EAFn \u0111\u1EC3\
  \ l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i. B\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng\
  \ h\xE0m t\u1EA1o `new Date()` \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.054657-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, d\u1EF1a tr\xEAn JavaScript, cung c\u1EA5p c\xE1c ph\u01B0\
  \u01A1ng ph\xE1p th\u1EB3ng th\u1EAFn \u0111\u1EC3 l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1\
  i."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

## Làm thế nào:
Google Apps Script, dựa trên JavaScript, cung cấp các phương pháp thẳng thắn để lấy ngày hiện tại. Bạn có thể sử dụng hàm tạo `new Date()` để tạo một đối tượng ngày mới đại diện cho ngày và giờ hiện tại. Dưới đây là cách bạn có thể thao tác và hiển thị điều này trong các định dạng khác nhau.

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // Ghi nhật ký ngày và giờ hiện tại theo múi giờ của mã
  
  // Hiển thị chỉ ngày ở định dạng YYYY-MM-DD
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // Ví dụ đầu ra: "2023-04-01"
  
  // Hiển thị ở định dạng dễ đọc hơn
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // Ví dụ đầu ra: "April 1, 2023, 12:00:00 PM GMT+1"
}
```

Những đoạn mã này minh họa cách ghi lại và định dạng ngày và giờ hiện tại, trình bày sự linh hoạt cho các nhu cầu lập trình khác nhau trong Google Apps Script.

## Tìm hiểu sâu
Trước khi JavaScript quyết định sử dụng đối tượng `Date`, lập trình viên phải tự mình theo dõi thời gian và ngày tháng thông qua các phương tiện không chuẩn và cồng kềnh hơn. Điều này bao gồm việc sử dụng số nguyên dấu thời gian và các hàm ngày tháng do chính họ tạo ra, có sự khác biệt từ môi trường lập trình này sang môi trường khác, dẫn đến sự không nhất quán và vấn đề tương thích.

Sự giới thiệu về đối tượng `new Date()` trong JavaScript, và do đó là trong Google Apps Script, đã chuẩn hóa các hoạt động liên quan đến ngày và giờ, làm cho chúng trở nên trực quan hơn và giảm lượng mã cần thiết cho các hoạt động liên quan đến ngày. Đáng chú ý là, mặc dù triển khai của Google Apps Script rất thuận tiện và đủ cho nhiều ứng dụng trong bộ sản phẩm của Google, nó có thể không đáp ứng được tất cả các tình huống, đặc biệt là những tình huống yêu cầu xử lý múi giờ phức tạp hoặc ghi dấu thời gian chính xác trong môi trường nhanh chóng.

Đối với những trường hợp sử dụng nâng cao như vậy, lập trình viên thường chuyển sang sử dụng các thư viện như Moment.js hoặc date-fns trong JavaScript. Mặc dù Google Apps Script không hỗ trợ trực tiếp những thư viện này, nhà phát triển có thể mô phỏng một số chức năng của chúng sử dụng các phương thức Date trong JavaScript có sẵn hoặc bằng cách truy cập các thư viện bên ngoài qua Dịch vụ HTML hoặc dịch vụ URL Fetch của Apps Script. Mặc dù có các lựa chọn thay thế, sự đơn giản và tích hợp của các hàm ngày và giờ bản địa của Google Apps Script vẫn là lựa chọn hàng đầu cho hầu hết các nhiệm vụ trong hệ sinh thái Google.
