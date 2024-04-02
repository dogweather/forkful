---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:15.268075-07:00
description: "Vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong JavaScript l\xE0\
  \ m\u1ED9t k\u1EF9 thu\u1EADt \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3\
  \ t\u1EA1o s\u1EF1 kh\xF4ng d\u1EF1 \u0111o\xE1n tr\u01B0\u1EDBc trong c\xE1c \u1EE9\
  ng d\u1EE5ng, t\u1EEB c\xE1c tr\xF2 ch\u01A1i c\u1EA7n h\xE0nh vi \u0111\u1ED1i\
  \ th\u1EE7\u2026"
lastmod: '2024-03-13T22:44:37.147824-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong JavaScript l\xE0 m\u1ED9\
  t k\u1EF9 thu\u1EADt \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 t\u1EA1\
  o s\u1EF1 kh\xF4ng d\u1EF1 \u0111o\xE1n tr\u01B0\u1EDBc trong c\xE1c \u1EE9ng d\u1EE5\
  ng, t\u1EEB c\xE1c tr\xF2 ch\u01A1i c\u1EA7n h\xE0nh vi \u0111\u1ED1i th\u1EE7\u2026"
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Gì và Tại sao?

Việc tạo số ngẫu nhiên trong JavaScript là một kỹ thuật được sử dụng để tạo sự không dự đoán trước trong các ứng dụng, từ các trò chơi cần hành vi đối thủ ngẫu nhiên đến các thuật toán bảo mật yêu cầu sự ngẫu nhiên mật mã hóa. Khả năng này rất quan trọng cho việc phát triển trải nghiệm người dùng động và các ứng dụng an toàn.

## Cách thức:

### Tạo Số Ngẫu Nhiên Cơ Bản

Cách đơn giản nhất để tạo một số ngẫu nhiên trong JavaScript là sử dụng `Math.random()`. Hàm này trả về một số thực nổi, số ngẫu nhiên giả mạo trong khoảng từ 0 (bao gồm) đến 1 (không bao gồm).

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### Tạo Số Ngẫu Nhiên Trong Một Khoảng

Thường thì bạn sẽ muốn một số nguyên ngẫu nhiên trong một khoảng cụ thể. Điều này có thể đạt được bằng cách chia tỷ lệ và làm tròn đầu ra của `Math.random()`.

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### Số Ngẫu Nhiên Bảo Mật Mật Mã Hóa

Đối với các ứng dụng cần một mức độ ngẫu nhiên cao hơn (ví dụ, các hoạt động mật mã hóa), phương pháp `crypto.getRandomValues()` có thể được sử dụng. Phương pháp này cung cấp sự ngẫu nhiên mật mã hóa, không giống như các số ngẫu nhiên giả mạo được tạo bởi `Math.random()`.

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## Sâu hơn nữa

Theo lịch sử, việc tạo số ngẫu nhiên trong JavaScript chỉ dựa vào hàm `Math.random()`. Mặc dù thuận tiện cho hầu hết các trường hợp sử dụng thông thường, thuật toán của nó, thường là một biến thể của một bộ sinh số ngẫu nhiên giả mạo (PRNG) như Mersenne Twister, không cung cấp an toàn mật mã hóa.

Sự giới thiệu của Web Cryptography API mang lại phương pháp `crypto.getRandomValues()`, cung cấp một cách để tạo ra những số ít dự đoán hơn và phù hợp cho các ứng dụng nhạy cảm với bảo mật. Phương pháp này tận dụng các nguồn ngẫu nhiên cơ bản của hệ điều hành, như `/dev/random` trên Unix/Linux, chúng mạnh mẽ hơn và phù hợp cho các hoạt động mật mã hóa.

Rất quan trọng khi chọn phương pháp đúng cho nhiệm vụ cần thực hiện. `Math.random()` đủ cho nhu cầu cơ bản như trò chơi đơn giản, hoạt hình, hoặc bất kỳ trường hợp nào mà chất lượng của sự ngẫu nhiên không phải là điều quan trọng. Tuy nhiên, cho các tính năng bảo mật, như mã token đặt lại mật khẩu hoặc bất kỳ hoạt động mật mã nào, `crypto.getRandomValues()` là sự lựa chọn tốt hơn do chất lượng ngẫu nhiên ưu việt hơn.

Đáng chú ý, `Math.random()` tạo ra các số với độ lệch biết trước trong hầu hết các triển khai, có nghĩa là một số số có khả năng xảy ra hơn những số khác. Mặc dù độ lệch này rất nhỏ và thường không đáng kể cho các ứng dụng chung, nó loại bỏ `Math.random()` khỏi việc được sử dụng trong bất kỳ ngữ cảnh mật mã hóa nào hoặc các ứng dụng nơi công bằng là quan trọng, như cờ bạc trực tuyến.

Kết luận, trong khi các hàm tích hợp sẵn của JavaScript cho việc tạo số ngẫu nhiên đều đáp ứng được một phạm vi rộng lớn nhu cầu, việc hiểu biết rõ sự khác biệt và hạn chế của từng phương pháp là cần thiết cho việc ứng dụng chúng một cách phù hợp.
