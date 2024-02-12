---
title:                "Viết hoa một chuỗi"
date:                  2024-01-28T21:55:44.365575-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết hoa một chuỗi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/javascript/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều gì & Tại sao?
Việc viết hoa một chuỗi nghĩa là thay đổi ký tự đầu tiên của một từ thành chữ cái in hoa. Các lập trình viên thực hiện điều này để tuân theo quy ước ngôn ngữ, cải thiện tính dễ đọc hoặc định dạng văn bản như tiêu đề.

## Làm thế nào:
JavaScript không có phương thức tích hợp sẵn để viết hoa, nhưng dưới đây là một hàm đơn giản thực hiện điều đó:

```javascript
function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}

console.log(capitalizeFirstLetter('hello')); // Kết quả: Hello
```

Đối với nhiều từ:

```javascript
function capitalizeWords(str) {
    return str.split(' ').map(capitalizeFirstLetter).join(' ');
}

console.log(capitalizeWords('hello world!')); // Kết quả: Hello World!
```

## Tìm hiểu sâu hơn
Việc viết hoa chuỗi không luôn có các hàm tích hợp sẵn trong các ngôn ngữ, thường liên quan đến việc thao tác ASCII thủ công. Ngày nay, hầu hết các ngôn ngữ lập trình đều cung cấp các phương thức để thao tác chuỗi, nhưng JavaScript yêu cầu một cách tiếp cận tự làm nhiều hơn.

### Các phương án thay thế:
Bạn có thể sử dụng CSS để viết hoa văn bản cho các trang web (`text-transform: capitalize;`), hoặc các thư viện như Lodash có các hàm viết hoa. Nhưng việc thực hiện bằng JavaScript thuần túy, như đã trình bày ở trên, không phụ thuộc vào bất kỳ thứ gì khác.

### Chi tiết thực hiện:
`charAt(0)` lấy ký tự đầu tiên. `toUpperCase()` chuyển nó thành chữ in hoa. Kết hợp nó với phần còn lại của chuỗi `slice(1)` để bạn có được một chuỗi được viết hoa. Phương pháp này hoạt động tốt giả sử đầu vào là một chuỗi và không bắt đầu bằng một khoảng trắng.

## Xem thêm:
- CSS text-transform của MDN để viết hoa: https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform
- Tài liệu về phương thức capitalize của Lodash: https://lodash.com/docs/4.17.15#capitalize
- JavaScript String.prototype.toUpperCase(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase
