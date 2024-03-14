---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:10.296555-07:00
description: "T\xE1i c\u1EA5u tr\xFAc l\xE0 qu\xE1 tr\xECnh c\u1EA3i t\u1ED5 m\xE3\
  \ m\xE1y t\xEDnh \u0111\xE3 t\u1ED3n t\u1EA1i m\xE0 kh\xF4ng thay \u0111\u1ED5i\
  \ h\xE0nh vi b\xEAn ngo\xE0i c\u1EE7a n\xF3. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 c\u1EA3i thi\u1EC7n c\xE1c\u2026"
lastmod: '2024-03-13T22:44:37.164523-06:00'
model: gpt-4-0125-preview
summary: "T\xE1i c\u1EA5u tr\xFAc l\xE0 qu\xE1 tr\xECnh c\u1EA3i t\u1ED5 m\xE3 m\xE1\
  y t\xEDnh \u0111\xE3 t\u1ED3n t\u1EA1i m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh\
  \ vi b\xEAn ngo\xE0i c\u1EE7a n\xF3. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c\
  \ hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 c\u1EA3i thi\u1EC7n c\xE1c\u2026"
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tái cấu trúc là quá trình cải tổ mã máy tính đã tồn tại mà không thay đổi hành vi bên ngoài của nó. Các lập trình viên thực hiện việc này để cải thiện các thuộc tính phi chức năng của phần mềm, làm cho mã sạch sẽ và hiệu quả hơn, giúp đơn giản hóa việc bảo trì và tạo điều kiện dễ dàng hơn cho việc thêm tính năng trong tương lai.

## Làm thế nào:

Hãy xem một ví dụ đơn giản về cách tái cấu trúc có thể làm cho mã của bạn gọn gàng và dễ đọc hơn. Tại đây, chúng ta tái cấu trúc một hàm tính tổng của một mảng số.

Trước:
```javascript
function calculateSum(arr) {
  let sum = 0;
  for (let i = 0; i < arr.length; i++) {
    sum += arr[i];
  }
  return sum;
}

console.log(calculateSum([1, 2, 3, 4])); // Kết quả: 10
```

Sau:
```javascript
function calculateSum(arr) {
  return arr.reduce((sum, num) => sum + num, 0);
}

console.log(calculateSum([1, 2, 3, 4])); // Kết quả: 10
```

Thấy cách phương thức `reduce` làm giảm kích thước của hàm mà vẫn giữ nguyên chức năng không? Đó chính là tái cấu trúc đó.

## Sâu hơn

Tái cấu trúc không xuất hiện như một thực hành chính thống cho đến khi cuốn sách "Refactoring: Improving the Design of Existing Code" của Martin Fowler được xuất bản vào năm 1999. Cuốn sách này, cùng với sự nổi lên của phát triển phần mềm linh hoạt, đã giúp đẩy tái cấu trúc vào chính thống.

Mô tả tái cấu trúc như một khía cạnh của phát triển phần mềm giống như giải thích lý do bạn dọn dẹp một xưởng: bạn làm vậy để lần sau khi bạn cần sửa chữa một thứ gì đó (trong trường hợp này là mã), bạn sẽ mất ít thời gian hơn để xử lý hỗn độn và chú trọng hơn vào vấn đề thực sự.

Khi chúng ta nói về các phương án thay thế cho việc tái cấu trúc, chúng ta bước vào một cuộc thảo luận rộng hơn về chiến lược bảo dưỡng phần mềm. Ví dụ, người ta có thể lựa chọn viết lại hoàn toàn, nhưng thường thì điều đó tốn kém và rủi ro hơn. Tái cấu trúc theo từng phần, và bạn sẽ nhận được lợi ích liên tục mà không làm đắm con thuyền bởi một cuộc đại tu đột ngột.

Tái cấu trúc đã được hỗ trợ bởi sự phát triển của các môi trường phát triển tích hợp (IDEs) và công cụ như JSHint, ESLint, và Prettier trong hệ sinh thái JavaScript, giúp tự động kiểm tra chất lượng mã và làm nổi bật các cơ hội cho tái cấu trúc.

Tất cả đều về mã sạch, biểu đạt và dễ bảo trì. Các thuật toán tinh vi, tối ưu hóa cấu trúc dữ liệu, hay thậm chí là thay đổi kiến trúc như chuyển đổi từ lập trình thủ tục sang lập trình hàm có thể là một phần của quá trình tái cấu trúc.

Tái cấu trúc phải được thực hiện một cách cẩn thận; việc có một bộ kiểm tra mạnh mẽ là rất quan trọng để đảm bảo rằng các thay đổi của bạn không làm thay đổi hành vi của phần mềm một cách không mong muốn - một lý do khác tại sao Phát triển Dựa trên Kiểm thử (TDD) kết hợp tốt với tái cấu trúc, vì nó cung cấp mạng lưới an toàn đó theo mặc định.

## Xem thêm

- Sách Refactoring của Martin Fowler: [Refactoring - Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Các Khung Kiểm thử JavaScript (để đảm bảo tái cấu trúc không làm hỏng chức năng):
  - Jest: [Jest - Kiểm thử JavaScript thú vị](https://jestjs.io/)
  - Mocha: [Mocha - khung kiểm thử JavaScript vui vẻ, đơn giản, linh hoạt](https://mochajs.org/)

- Công cụ Hỗ trợ Chất lượng Mã và Tái Cấu Trúc:
  - ESLint: [ESLint - Linter JavaScript có thể cắm vào](https://eslint.org/)
  - Prettier: [Prettier - Định dạng Mã theo Ý kiến](https://prettier.io/)
