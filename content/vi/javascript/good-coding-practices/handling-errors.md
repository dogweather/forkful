---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:40.063854-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 kh\u1ED1\
  i `try-catch` kinh \u0111i\u1EC3n."
lastmod: '2024-03-13T22:44:37.163064-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 kh\u1ED1i `try-catch` kinh \u0111i\u1EC3\
  n."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

## Cách thực hiện:
Dưới đây là khối `try-catch` kinh điển:

```javascript
try {
  // Mã có thể phát ra lỗi
  let result = potentiallyRiskyOperation();
  console.log('Thành công:', result);
} catch (error) {
  // Cần làm gì nếu một lỗi xảy ra
  console.error('Ối:', error.message);
}
```

Đầu ra mẫu khi không có lỗi xảy ra:
```
Thành công: 42
```

Và khi có lỗi:
```
Ối: Đã xảy ra sự cố
```

Đối với mã bất đồng bộ, nơi đòi hỏi sử dụng promises, hãy dùng `try-catch` trong một hàm `async`:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Dữ liệu đã được tải:', data);
  } catch (error) {
    console.error('Lỗi khi tải dữ liệu:', error.message);
  }
}

fetchData();
```

## Sâu hơn
Xử lý lỗi trong JavaScript đã phát triển. Ngày xưa (ES3, khoảng năm 1999), chúng ta chỉ có khối `try-catch`. Không quá linh hoạt, nhưng nó đã hoàn thành công việc.

ES6 (2015) đã giới thiệu Promises và tặng chúng ta `.then()` và `.catch()`, cho phép chúng ta xử lý lỗi bất đồng bộ một cách nhẹ nhàng hơn.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Dữ liệu đã được tải:', data))
  .catch(error => console.error('Lỗi khi tải dữ liệu:', error.message));
```

Về chi tiết triển khai, khi một lỗi được phát ra, engine JavaScript tạo ra một đối tượng `Error` với những thuộc tính hữu ích như `message` và `stack`. Bạn cũng có thể tạo ra các loại lỗi tùy chỉnh bằng cách mở rộng lớp `Error` - rất tiện lợi cho những ứng dụng phức tạp hơn.

Những phương án khác? Bạn có thể bỏ qua xử lý lỗi (không phải là ý tưởng hay), sử dụng callbacks với tham số lỗi đầu tiên (xin chào, phong cách Node.js), hoặc trở nên tinh tế hơn với các thư viện và frameworks cung cấp quan điểm của chúng.

## Xem thêm
Để hiểu thêm về xử lý lỗi:

- MDN về try-catch: [MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN hàm async](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- Hướng dẫn về Promises: [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Tạo và phát lỗi tùy chỉnh: [MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
