---
title:                "TypeScript: 编写测试"
simple_title:         "编写测试"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/writing-tests.md"
---

{{< edit_this_page >}}

Why: 为什么要编写测试？
测试是软件开发过程中至关重要的一步。它可以帮助您发现并修复潜在的错误，提高代码的质量和可靠性。编写测试可以让您的代码更加健壮，并且在未来的开发过程中节省更多的时间和精力。

How To: 如何编写测试 

```TypeScript
// 让我们从一个简单的加法函数开始
function add(num1: number, num2: number): number {
  return num1 + num2;
}

// 创建一个单元测试以验证函数的正确性
test("add函数应该正确计算两个数字的和", () => {
  // 准备测试所需的输入
  const num1 = 10;
  const num2 = 5;
  // 调用函数，获取结果
  const result = add(num1, num2);
  // 断言，判断结果是否等于预期值
  expect(result).toBe(15);
});

// 运行测试，可以看到输出结果
// add函数应该正确计算两个数字的和
// ✓
```

Deep Dive: 关于编写测试的更多信息
1. 使用断言函数来验证结果的正确性，例如使用`expect`和`toBe`来比较预期值和实际值。
2. 在编写测试时，应该考虑边界条件和特殊情况，以确保代码的完整性。
3. 使用`beforeAll`和`afterAll`来在测试前后进行准备和清理工作，例如创建和销毁临时数据库。
4. 结合使用测试覆盖率工具，可以帮助您找到未覆盖的代码，进一步提高测试的全面性。
5. 不要滥用单元测试，识别出哪些函数或组件是最需要被测试的，以避免写出低价值的测试用例。

See Also: 查看更多关于编写测试的资料和示例
- [TypeScript手册：测试](https://www.typescriptlang.org/docs/handbook/testing.html)
- [Jest官方文档](https://jestjs.io/docs/zh-Hans/getting-started)
- [单元测试从入门到精通](https://juejin.im/post/5c0946a551882509ac2cb072)