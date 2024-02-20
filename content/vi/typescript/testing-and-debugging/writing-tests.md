---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:32.555306-07:00
description: "Vi\u1EBFt ki\u1EC3m th\u1EED ngh\u0129a l\xE0 t\u1EA1o ra m\xE3 l\u1EC7\
  nh \u0111\u1EC3 ki\u1EC3m tra xem m\xE3 l\u1EC7nh kh\xE1c c\xF3 ho\u1EA1t \u0111\
  \u1ED9ng \u0111\xFAng kh\xF4ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m v\u1EAD\
  y \u0111\u1EC3 ph\xE1t hi\u1EC7n l\u1ED7i s\u1EDBm, ti\u1EBFt ki\u1EC7m\u2026"
lastmod: 2024-02-19 22:04:55.474009
model: gpt-4-0125-preview
summary: "Vi\u1EBFt ki\u1EC3m th\u1EED ngh\u0129a l\xE0 t\u1EA1o ra m\xE3 l\u1EC7\
  nh \u0111\u1EC3 ki\u1EC3m tra xem m\xE3 l\u1EC7nh kh\xE1c c\xF3 ho\u1EA1t \u0111\
  \u1ED9ng \u0111\xFAng kh\xF4ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m v\u1EAD\
  y \u0111\u1EC3 ph\xE1t hi\u1EC7n l\u1ED7i s\u1EDBm, ti\u1EBFt ki\u1EC7m\u2026"
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
---

{{< edit_this_page >}}

## Gì & Tại sao?

Viết kiểm thử nghĩa là tạo ra mã lệnh để kiểm tra xem mã lệnh khác có hoạt động đúng không. Các lập trình viên làm vậy để phát hiện lỗi sớm, tiết kiệm thời gian, và đảm bảo các thay đổi không làm hỏng mọi thứ.

## Làm thế nào:

Hãy kiểm tra một hàm đơn giản sử dụng Jest, một khung kiểm thử phổ biến cho JavaScript và TypeScript.

Đầu tiên, cài đặt Jest với hỗ trợ TypeScript:

```bash
npm install --save-dev jest @types/jest ts-jest
```

Thêm một file `jest.config.js`:

```js
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

Định nghĩa một hàm trong `math.ts`:

```typescript
export function add(a: number, b: number): number {
  return a + b;
}
```

Viết kiểm thử trong `math.test.ts`:

```typescript
import { add } from './math';

test('cộng 1 + 2 để bằng 3', () => {
  expect(add(1, 2)).toBe(3);
});
```

Chạy kiểm thử:

```bash
npx jest
```

Kết quả mẫu:

```
PASS  ./math.test.ts
✓ cộng 1 + 2 để bằng 3 (5ms)
```

## Khám Phá Sâu

Kiểm thử trong TypeScript dựa trên thực hành kiểm thử JavaScript. Dưới đây là những điều làm cho nó đặc biệt:

- Bối cảnh lịch sử: TypeScript ra đời vào năm 2012. Mục đích là để thêm các kiểu dữ liệu vào JavaScript, giúp mã lệnh dễ bảo trì và kiểm thử hơn.
- Các lựa chọn khác: Ngoài Jest, còn có Mocha, Jasmine, và nhiều hơn nữa. Mỗi cái có những đặc điểm độc đáo; chọn dựa trên nhu cầu của bạn.
- Chi tiết triển khai: Các bài kiểm thử có thể sống cùng với mã hoặc riêng biệt. Các kiểu dữ liệu TypeScript giúp với việc tự động hoàn thành và tăng thêm sự tự tin trong kiểm thử.

## Xem Thêm

- Jest: [Tài liệu Jest](https://jestjs.io/docs/getting-started)
- So sánh Khung Kiểm Thử JS: [Khảo sát StateOfJS 2022](https://2022.stateofjs.com/en-US/libraries/testing/)
