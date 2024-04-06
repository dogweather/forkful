---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:53.217999-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A TypeScript \u4E0E\u5927\u591A\u6570 JavaScript\
  \ \u6D4B\u8BD5\u6846\u67B6\u548C\u8C10\u5DE5\u4F5C\u3002\u4E3A\u4E86\u6F14\u793A\
  \u76EE\u7684\uFF0C\u6211\u4EEC\u5C06\u4F7F\u7528 Jest\uFF0C\u4E00\u4E2A\u6D41\u884C\
  \u7684\u6D4B\u8BD5\u6846\u67B6\uFF0C\u56E0\u4E3A\u5B83\u4E3A TypeScript \u9879\u76EE\
  \u63D0\u4F9B\u4E86\u96F6\u914D\u7F6E\u8BBE\u7F6E\u3002 \u9996\u5148\uFF0C\u786E\u4FDD\
  \u60A8\u5DF2\u5B89\u88C5 Jest \u548C\u5FC5\u8981\u7684 TypeScript \u7C7B\u578B\uFF1A\
  ."
lastmod: '2024-04-05T21:53:47.799740-06:00'
model: gpt-4-0125-preview
summary: "\u9996\u5148\uFF0C\u786E\u4FDD\u60A8\u5DF2\u5B89\u88C5 Jest \u548C\u5FC5\
  \u8981\u7684 TypeScript \u7C7B\u578B\uFF1A."
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

## 如何操作：
TypeScript 与大多数 JavaScript 测试框架和谐工作。为了演示目的，我们将使用 Jest，一个流行的测试框架，因为它为 TypeScript 项目提供了零配置设置。

首先，确保您已安装 Jest 和必要的 TypeScript 类型：

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

接下来，设置 Jest 以便与 TypeScript 协作，通过修改 `jest.config.js` 或如果是创建一个新的：

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

现在，让我们编写一个简单的函数和对它的测试。考虑一个有以下函数的 `sum.ts` 文件：

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

创建一个名为 `sum.test.ts` 的测试文件：

```typescript
// sum.test.ts
import { sum } from './sum';

test('1 + 2 加起来等于 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

使用以下命令运行您的测试：

```bash
npx jest
```

显示测试通过的样本输出应如下所示：

```plaintext
 PASS  ./sum.test.ts
  ✓ 1 + 2 加起来等于 3 (2 ms)
```

对于异步代码，Jest 提供了 `async/await` 支持。假设您有一个异步的 `fetchData` 函数：

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

使用异步函数的测试：

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('成功获取数据', async () => {
  expect(await fetchData()).toBe('data');
});
```

当运行您的测试时，Jest 将等待 promise 解决，正确地测试异步操作。

记住，有效的测试包括为不同的场景编写多个测试，包括边缘情况，以确保您的 TypeScript 代码按预期工作。
