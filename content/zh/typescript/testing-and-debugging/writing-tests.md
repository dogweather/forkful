---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:53.217999-07:00
description: "\u4F7F\u7528 TypeScript \u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\
  \u81EA\u52A8\u5316\u811A\u672C\u6765\u9A8C\u8BC1\u4EE3\u7801\u7684\u529F\u80FD\u6027\
  \u548C\u6B63\u786E\u6027\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\
  \u786E\u4FDD\u53EF\u9760\u6027\uFF0C\u5FEB\u901F\u6355\u6349\u5230 bug\uFF0C\u5E76\
  \u4FC3\u8FDB\u4EE3\u7801\u7684\u53EF\u7EF4\u62A4\u6027\u589E\u957F\uFF0C\u56E0\u4E3A\
  \ TypeScript \u7684\u9759\u6001\u7C7B\u578B\u6DFB\u52A0\u4E86\u4E00\u5C42\u53EF\u9884\
  \u6D4B\u6027\u5230 JavaScript \u6D4B\u8BD5\u4E2D\u3002"
lastmod: '2024-03-13T22:44:47.475811-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u7528 TypeScript \u7F16\u5199\u6D4B\u8BD5\u6D89\u53CA\u521B\u5EFA\
  \u81EA\u52A8\u5316\u811A\u672C\u6765\u9A8C\u8BC1\u4EE3\u7801\u7684\u529F\u80FD\u6027\
  \u548C\u6B63\u786E\u6027\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\
  \u786E\u4FDD\u53EF\u9760\u6027\uFF0C\u5FEB\u901F\u6355\u6349\u5230 bug\uFF0C\u5E76\
  \u4FC3\u8FDB\u4EE3\u7801\u7684\u53EF\u7EF4\u62A4\u6027\u589E\u957F\uFF0C\u56E0\u4E3A\
  \ TypeScript \u7684\u9759\u6001\u7C7B\u578B\u6DFB\u52A0\u4E86\u4E00\u5C42\u53EF\u9884\
  \u6D4B\u6027\u5230 JavaScript \u6D4B\u8BD5\u4E2D\u3002"
title: "\u7F16\u5199\u6D4B\u8BD5"
---

{{< edit_this_page >}}

## 什么 & 为什么？
使用 TypeScript 编写测试涉及创建自动化脚本来验证代码的功能性和正确性。程序员这样做是为了确保可靠性，快速捕捉到 bug，并促进代码的可维护性增长，因为 TypeScript 的静态类型添加了一层可预测性到 JavaScript 测试中。

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
