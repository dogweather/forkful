---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:32.555306-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y ki\u1EC3m tra m\u1ED9t h\xE0m \u0111\u01A1\
  n gi\u1EA3n s\u1EED d\u1EE5ng Jest, m\u1ED9t khung ki\u1EC3m th\u1EED ph\u1ED5 bi\u1EBF\
  n cho JavaScript v\xE0 TypeScript. \u0110\u1EA7u ti\xEAn, c\xE0i \u0111\u1EB7t Jest\
  \ v\u1EDBi h\u1ED7 tr\u1EE3\u2026"
lastmod: '2024-03-13T22:44:36.323741-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y ki\u1EC3m tra m\u1ED9t h\xE0m \u0111\u01A1n gi\u1EA3n s\u1EED d\u1EE5\
  ng Jest, m\u1ED9t khung ki\u1EC3m th\u1EED ph\u1ED5 bi\u1EBFn cho JavaScript v\xE0\
  \ TypeScript."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

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
