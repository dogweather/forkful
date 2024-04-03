---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:10.296555-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y xem m\u1ED9t v\xED d\u1EE5 \u0111\u01A1\
  n gi\u1EA3n v\u1EC1 c\xE1ch t\xE1i c\u1EA5u tr\xFAc c\xF3 th\u1EC3 l\xE0m cho m\xE3\
  \ c\u1EE7a b\u1EA1n g\u1ECDn g\xE0ng v\xE0 d\u1EC5 \u0111\u1ECDc h\u01A1n. T\u1EA1\
  i \u0111\xE2y, ch\xFAng ta t\xE1i c\u1EA5u tr\xFAc m\u1ED9t h\xE0m t\xEDnh\u2026"
lastmod: '2024-03-13T22:44:37.164523-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y xem m\u1ED9t v\xED d\u1EE5 \u0111\u01A1n gi\u1EA3n v\u1EC1 c\xE1\
  ch t\xE1i c\u1EA5u tr\xFAc c\xF3 th\u1EC3 l\xE0m cho m\xE3 c\u1EE7a b\u1EA1n g\u1ECD\
  n g\xE0ng v\xE0 d\u1EC5 \u0111\u1ECDc h\u01A1n."
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
weight: 19
---

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
