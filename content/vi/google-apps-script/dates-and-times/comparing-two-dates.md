---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:20.546937-07:00
description: "So s\xE1nh hai ng\xE0y trong Google Apps Script, m\u1ED9t phi\xEAn b\u1EA3\
  n \u0111\u01B0\u1EE3c t\xF9y ch\u1EC9nh c\u1EE7a JavaScript d\xE0nh cho b\u1ED9\
  \ \u1EE9ng d\u1EE5ng c\u1EE7a Google, l\xE0 m\u1ED9t nhi\u1EC7m v\u1EE5 quan tr\u1ECD\
  ng \u0111\u1ED1i v\u1EDBi\u2026"
lastmod: '2024-02-25T18:49:34.432063-07:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y trong Google Apps Script, m\u1ED9t phi\xEAn b\u1EA3\
  n \u0111\u01B0\u1EE3c t\xF9y ch\u1EC9nh c\u1EE7a JavaScript d\xE0nh cho b\u1ED9\
  \ \u1EE9ng d\u1EE5ng c\u1EE7a Google, l\xE0 m\u1ED9t nhi\u1EC7m v\u1EE5 quan tr\u1ECD\
  ng \u0111\u1ED1i v\u1EDBi\u2026"
title: "So s\xE1nh hai ng\xE0y"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
So sánh hai ngày trong Google Apps Script, một phiên bản được tùy chỉnh của JavaScript dành cho bộ ứng dụng của Google, là một nhiệm vụ quan trọng đối với các nhà phát triển khi xử lý lịch trình, dòng thời gian, hoặc bất kỳ dữ liệu nào liên quan đến ngày tháng. Hiểu cách so sánh các ngày một cách chính xác giúp lập trình viên thực hiện các tính năng như hạn chót, lên kế hoạch sự kiện, hoặc lên lịch nội dung một cách hiệu quả.

## Làm thế nào:
Trong Google Apps Script, các ngày được so sánh sử dụng các đối tượng Date của JavaScript, cho phép sử dụng các phương pháp tiêu chuẩn để đánh giá xem ngày nào trước, ngày nào sau, hoặc nếu chúng giống nhau. Dưới đây là một cách tiếp cận cơ bản:

```javascript
function compareDates() {
  var date1 = new Date('2023-04-01T00:00:00');
  var date2 = new Date('2023-04-15T00:00:00');

  // So sánh các ngày
  if (date1 < date2) {
    Logger.log('Date1 trước Date2');
  } else if (date1 > date2) {
    Logger.log('Date1 sau Date2');
  } else {
    Logger.log('Cả hai ngày giống nhau');
  }
}

// Kết quả mẫu:
// Date1 trước Date2
```

Đối với những so sánh chi tiết hơn (như số ngày giữa hai ngày), bạn có thể trừ một ngày cho ngày kia, cái này sẽ trả về sự khác biệt tính bằng mili giây:

```javascript
function daysBetweenDates() {
  var date1 = new Date('2023-04-01');
  var date2 = new Date('2023-04-15');
  
  var difference = date2 - date1;
  
  var days = difference / (1000 * 60 * 60 * 24); // Chuyển đổi mili giây sang ngày
  Logger.log(days + ' ngày giữa các ngày');
}

// Kết quả mẫu:
// 14 ngày giữa các ngày
```

## Đi sâu hơn
Google Apps Script áp dụng các nguyên tắc cơ bản của các đối tượng Date trong JavaScript cho việc so sánh ngày, cái đã là một phần quan trọng của ngôn ngữ kể từ khi nó ra đời. Sử dụng mili giây làm giá trị so sánh kể từ Epoch Unix (ngày 1 tháng 1 năm 1970) cung cấp mức độ chính xác cao để xác định sự khác biệt hoặc điểm giống nhau giữa các ngày.

Mặc dù cách tiếp cận này hiệu quả cho hầu hết các trường hợp sử dụng trong phạm vi của Google Apps Script, nhưng cần lưu ý rằng các thao tác trên ngày — như điều chỉnh múi giờ và tính toán năm nhuận — đôi khi có thể gây nhầm lẫn. Các nhà phát triển từ các nền tảng lập trình khác (như Python, nơi các mô-đun `datetime` và `dateutil` cung cấp cách xử lý ngày tháng tinh vi hơn) có thể thấy đối tượng Date của JavaScript thiếu tính năng.

Đối với việc xử lý và điều chỉnh ngày tháng phức tạp hơn so với so sánh đơn giản, các thư viện như `Moment.js` (vẫn có thể được sử dụng trong Google Apps Script thông qua API bên ngoài) cung cấp một bộ chức năng phong phú giải quyết những thiếu sót này. Tuy nhiên, đối tượng Date của JavaScript vẫn tiếp tục là một công cụ đáng tin cậy cho hầu hết các nhiệm vụ so sánh ngày, đặc biệt trong bối cảnh của Google Apps Script và sự tích hợp của nó với bộ ứng dụng của Google.
