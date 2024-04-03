---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:24.553353-07:00
description: "L\xE0m th\u1EBF n\xE0o: M\u1EB7c d\xF9 Google Apps Script kh\xF4ng c\xF3\
  \ m\u1ED9t khung ki\u1EC3m th\u1EED t\xEDch h\u1EE3p s\u1EB5n nh\u01B0 m\u1ED9t\
  \ s\u1ED1 m\xF4i tr\u01B0\u1EDDng l\u1EADp tr\xECnh kh\xE1c, b\u1EA1n v\u1EABn c\xF3\
  \ th\u1EC3 vi\u1EBFt v\xE0 ch\u1EA1y c\xE1c b\xE0i\u2026"
lastmod: '2024-03-13T22:44:36.045265-06:00'
model: gpt-4-0125-preview
summary: "M\u1EB7c d\xF9 Google Apps Script kh\xF4ng c\xF3 m\u1ED9t khung ki\u1EC3\
  m th\u1EED t\xEDch h\u1EE3p s\u1EB5n nh\u01B0 m\u1ED9t s\u1ED1 m\xF4i tr\u01B0\u1EDD\
  ng l\u1EADp tr\xECnh kh\xE1c, b\u1EA1n v\u1EABn c\xF3 th\u1EC3 vi\u1EBFt v\xE0 ch\u1EA1\
  y c\xE1c b\xE0i test b\u1EB1ng c\xE1ch t\u1EADn d\u1EE5ng c\xE1c h\xE0m GAS \u0111\
  \u01A1n gi\u1EA3n ho\u1EB7c t\xEDch h\u1EE3p c\xE1c th\u01B0 vi\u1EC7n ki\u1EC3\
  m th\u1EED b\xEAn ngo\xE0i nh\u01B0 `QUnit`."
title: "Vi\u1EBFt ki\u1EC3m th\u1EED"
weight: 36
---

## Làm thế nào:
Mặc dù Google Apps Script không có một khung kiểm thử tích hợp sẵn như một số môi trường lập trình khác, bạn vẫn có thể viết và chạy các bài test bằng cách tận dụng các hàm GAS đơn giản hoặc tích hợp các thư viện kiểm thử bên ngoài như `QUnit`. Đây là một ví dụ cơ bản sử dụng một hàm GAS đơn giản để kiểm tra một hàm khác trong script của bạn:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("Kiểm thử thất bại: add(2, 3) phải là 5, nhưng lại là " + result);
  } else {
    Logger.log("Kiểm thử thành công!");
  }
}
```

Chạy `testAdd()` sẽ log "Kiểm thử thành công!" nếu hàm `add` hoạt động chính xác, hoặc ném ra lỗi nếu không. Để áp dụng một cách tiếp cận tinh vi hơn, tích hợp QUnit với Google Apps Script đòi hỏi một vài bước thêm nhưng cung cấp một môi trường kiểm thử mạnh mẽ. Một ví dụ về cài đặt kiểm thử QUnit trông như thế này:

1. Bao gồm thư viện QUnit trong dự án của bạn.
2. Tạo một tệp HTML kiểm thử cho việc chạy các kiểm thử QUnit.
3. Viết các trường hợp kiểm thử sử dụng cú pháp của QUnit.

Đây là một ví dụ sử dụng QUnit:

```javascript
// Bao gồm QUnit bằng cách liên kết đến nó trong một tệp HTML được sử dụng để chạy các bài kiểm thử của bạn

QUnit.test("Kiểm thử hàm add", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3) phải trả về 5");
});
```

Để xem kết quả, mở tệp HTML trong Trình chỉnh sửa Script GAS hoặc triển khai nó như một ứng dụng web.

## Sâu hơn
Lịch sử, việc kiểm thử trong Google Apps Script đã được coi nhẹ phần nào, có thể do nguồn gốc và các trường hợp sử dụng chính của nền tảng tập trung vào các tác vụ tự động hóa nhanh chóng, quy mô nhỏ hơn thay vì các ứng dụng lớn. Do đó, GAS không cung cấp các khung kiểm thử và công cụ mạnh mẽ như những môi trường lập trình truyền thống khác. Tuy nhiên, cộng đồng đã thích nghi bằng cách kết hợp các thư viện mã nguồn mở và sử dụng sáng tạo các công cụ hiện có của Google.

Sử dụng các thư viện như QUnit đánh dấu một bước tiến quan trọng nhưng cũng đặt ra bộ thách thức riêng của nó, như thiết lập một môi trường kiểm thử phù hợp và học một cú pháp bổ sung. Tuy nhiên, đối với những người quan tâm đến việc xây dựng các ứng dụng phức tạp và đáng tin cậy hơn với GAS, nỗ lực này là đáng giá.

Các phương thức thay thế như sử dụng các hàm GAS đơn giản cho việc kiểm thử cung cấp sự dễ dàng trong sử dụng và tích hợp với môi trường GAS mà không cần thêm phụ thuộc nhưng thiếu đi các tính năng kiểm thử toàn diện và khả năng mở rộng dễ dàng khi dự án phát triển. Công cụ như clasp (Google Apps Script Command Line Interface) có thể tạo điều kiện cho các luồng công việc tiên tiến hơn, bao gồm cả việc kiểm thử, bằng cách cho phép các nhà phát triển lập trình trong IDE ưa thích của họ, giới thiệu khả năng tích hợp mạnh mẽ hơn với các khung kiểm thử bên ngoài.

Kết luận, mặc dù GAS có thể không có sẵn hỗ trợ gốc cho việc kiểm thử tinh vi ngay từ đầu, nhưng sự linh hoạt và các cách tiếp cận sáng tạo của cộng đồng cung cấp các lối đi khả thi để đảm bảo script của bạn mạnh mẽ, đáng tin cậy và sẵn sàng cho mọi nhiệm vụ.
