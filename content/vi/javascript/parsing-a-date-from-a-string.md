---
title:                "Phân tích ngày từ chuỗi kí tự"
aliases:
- vi/javascript/parsing-a-date-from-a-string.md
date:                  2024-01-28T22:04:40.478475-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Phân tích cú pháp một ngày từ một chuỗi có nghĩa là chuyển đổi văn bản thể hiện một ngày thành một đối tượng ngày. Lập trình viên làm điều này bởi vì nó quan trọng để xử lý ngày trong các ứng dụng, như sắp xếp các sự kiện hoặc lọc nhật ký.

## Làm thế nào:

Trong JavaScript, bạn có thể phân tích cú pháp một ngày từ một chuỗi sử dụng `Date` constructor hoặc các thư viện như `Date-fns` và `Moment.js`. Dưới đây là cách làm bằng cách dùng nguyên bản:

```Javascript
let dateString = "2023-04-01T12:00:00Z";
let parsedDate = new Date(dateString);

console.log(parsedDate); // Xuất ra: Sat Apr 01 2023 12:00:00 GMT+0000 (Thời gian phối hợp toàn cầu)
```

Để kiểm soát và nhất quán hơn, thư viện có thể hữu ích:

```Javascript
// Phân tích cú pháp sử dụng Moment.js
const moment = require('moment');
let momentDate = moment("2023-04-01");
console.log(momentDate.toString()); // Xuất ra: Sat Apr 01 2023 00:00:00 GMT+0000

// Phân tích cú pháp sử dụng Date-fns
const dateFns = require('date-fns/parse');
let dateFnsDate = dateFns("2023-04-01", "yyyy-MM-dd", new Date());
console.log(dateFnsDate); // Xuất ra: Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)
```

## Đi Sâu

JavaScript có khả năng xử lý ngày được tích hợp sẵn, nhưng nó không phải lúc nào cũng tốt. Các phiên bản đầu có vấn đề về nhất quán, múi giờ, và định dạng. Mọi người thường cảm thấy bực bội và xây dựng các giải pháp riêng của mình hoặc sử dụng thư viện bên thứ ba như `Moment.js` thì có nhiều tính năng và lựa chọn phân tích cú pháp tốt hơn.

Theo thời gian, JavaScript được cải thiện, và các thư viện mới như `Date-fns` và `Luxon` xuất hiện, tập trung vào các tiện ích nhỏ hơn, nhanh hơn và có thể mô đun hóa. Một lựa chọn khác là `Intl.DateTimeFormat` constructor, một phần của Internationalization API, cho phép định dạng ngày và giờ nhạy cảm với ngôn ngữ.

Đây là bản chất: phân tích cú pháp là rủi ro do sự khác biệt về định dạng. `Date` constructor trong JavaScript có thể hành xử không dự đoán được với các chuỗi ngày mơ hồ. Tốt nhất là sử dụng một định dạng tiêu chuẩn như ISO 8601 (`YYYY-MM-DDTHH:mm:ss.sssZ`) để tránh nhầm lẫn. Thư viện đi kèm với các quy tắc phân tích cú pháp của riêng mình và các chức năng bổ sung để xử lý các tinh tế của các định dạng ngày-giờ để nhà phát triển có thể tránh những điểm yếu phổ biến.

Nhớ luôn cảnh giác với múi giờ khi phân tích cú pháp ngày; chúng có thể tạo nên hoặc phá vỡ sự chính xác của logic ngày của bạn.

## Xem Thêm

- Tài liệu MDN Web về `Date`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js: https://momentjs.com/docs/#/parsing/string/
- Tài liệu Date-fns: https://date-fns.org/v2.28.0/docs/parse
- Internationalization API: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat
