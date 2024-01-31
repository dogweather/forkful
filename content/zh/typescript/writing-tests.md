---
title:                "编写测试代码"
date:                  2024-01-19
simple_title:         "编写测试代码"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
编写测试是创建测试案例以检查代码正确性的过程。程序员这么做是为了提前发现bug，确保程序行为符合预期，提升代码质量。

## How to: (如何操作：)
以下是TypeScript中使用Jest进行单元测试的一个例子：

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}

// sum.test.ts
import { sum } from './sum';

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```
运行测试，输出如下：
```
PASS ./sum.test.ts
  ✓ adds 1 + 2 to equal 3 (5ms)
```

## Deep Dive (深入探究)
写测试的历史可以追溯到软件发展的早期阶段。TypeScript出现后，测试框架如Mocha, Jasmine和Jest等逐渐支持TypeScript语法。选择哪个框架，取决于个人喜好与项目需求，但Jest因其易用性和功能性而备受欢迎。编写测试时，应注意遵循AAA（Arrange-Act-Assert）原则，确保测试逻辑清晰。

## See Also (另请参阅)
- Jest官方文档: [Jest](https://jestjs.io/docs/getting-started)
- 测试实践指南: [Test-Driven Development](https://www.martinfowler.com/bliki/TestDrivenDevelopment.html)
