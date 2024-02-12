---
title:                "Viết các bài kiểm tra"
aliases: - /vi/typescript/writing-tests.md
date:                  2024-01-28T22:13:32.555306-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
