---
title:                "编写测试"
aliases:
- zh/typescript/writing-tests.md
date:                  2024-02-03T19:32:53.217999-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
