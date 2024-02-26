---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:24.553353-07:00
description: "Vi\u1EBFt test trong Google Apps Script (GAS) l\xE0 v\u1EC1 vi\u1EC7\
  c t\u1EA1o ra c\xE1c script t\u1EF1 \u0111\u1ED9ng \u0111\u1EC3 x\xE1c minh h\xE0\
  nh vi c\u1EE7a m\xE3 l\u1EC7nh c\u1EE7a b\u1EA1n, \u0111\u1EA3m b\u1EA3o ch\xFA\
  ng ho\u1EA1t \u0111\u1ED9ng nh\u01B0 mong\u2026"
lastmod: '2024-02-25T18:49:34.420216-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt test trong Google Apps Script (GAS) l\xE0 v\u1EC1 vi\u1EC7c t\u1EA1\
  o ra c\xE1c script t\u1EF1 \u0111\u1ED9ng \u0111\u1EC3 x\xE1c minh h\xE0nh vi c\u1EE7\
  a m\xE3 l\u1EC7nh c\u1EE7a b\u1EA1n, \u0111\u1EA3m b\u1EA3o ch\xFAng ho\u1EA1t \u0111\
  \u1ED9ng nh\u01B0 mong\u2026"
title: "Vi\u1EBFt ki\u1EC3m th\u1EED"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Viết test trong Google Apps Script (GAS) là về việc tạo ra các script tự động để xác minh hành vi của mã lệnh của bạn, đảm bảo chúng hoạt động như mong đợi dưới các điều kiện khác nhau. Các lập trình viên làm điều này để bắt lỗi sớm, cải thiện chất lượng mã lệnh, và tạo điều kiện dễ dàng hơn cho việc cập nhật và bảo trì.

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
