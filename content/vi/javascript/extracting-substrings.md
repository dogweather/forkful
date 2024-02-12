---
title:                "Trích xuất chuỗi con"
aliases:
- vi/javascript/extracting-substrings.md
date:                  2024-01-28T21:59:44.189405-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trích xuất chuỗi con"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Trích xuất chuỗi con có nghĩa là lấy một phần của chuỗi. Lập trình viên cắt và chia chuỗi để cô lập dữ liệu, nhập một số định dạng, hoặc chỉnh sửa văn bản trước khi xuất.

## Cách thực hiện:

### Sử dụng phương thức `substring`:
```javascript
let text = "JavaScript is awesome!";
let extracted = text.substring(0, 10);
console.log(extracted); // Đầu ra: JavaScript
```

### Sử dụng phương thức `slice`:
```javascript
let text = "JavaScript is awesome!";
let sliced = text.slice(-9, -1);
console.log(sliced); // Đầu ra: awesome
```

### Sử dụng phương thức `substr` (đã được đánh dấu là không nên dùng):
```javascript
let text = "JavaScript is awesome!";
let substrd = text.substr(11, 7);
console.log(substrd); // Đầu ra: awesome
```

## Đi sâu hơn
Việc trích xuất chuỗi con không phải là mới – nó cũ như chính lập trình. Các phương thức `substring` và `slice` trong JavaScript là những công cụ từ những năm 1990, là một phần của bộ tính năng ban đầu của ngôn ngữ. `substr` cũng nằm trong số đó, nhưng hiện tại nó đã trở thành mã nguồn lệ và nên được tránh sử dụng trong các ứng dụng hiện đại.

Sự khác biệt? `substring` và `slice` tương tự nhau – cả hai đều nhận tham số chỉ số bắt đầu và kết thúc – nhưng xử lý số âm khác nhau: `slice` có thể xử lý chỉ số âm, đếm từ phía sau, trong khi `substring` coi chúng như là số không. Tất cả các phương thức này đều không thay đổi chuỗi gốc; chúng tạo ra các chuỗi mới.

## Xem thêm
- Mạng Lưới Nhà Phát Triển Mozilla về Chuỗi: [MDN Web Docs - String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- Manipulation chuỗi với JavaScript: [W3Schools - Phương thức Chuỗi JavaScript](https://www.w3schools.com/js/js_string_methods.asp)
- Cơ bản về chuỗi JavaScript: [JavaScript.info - Chuỗi](https://javascript.info/string)
