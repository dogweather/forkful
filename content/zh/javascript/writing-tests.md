---
title:                "编写测试代码"
date:                  2024-01-19
html_title:           "Arduino: 编写测试代码"
simple_title:         "编写测试代码"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/writing-tests.md"
---

{{< edit_this_page >}}

# 写测试：是什么，为什么？
编写测试意味着为你的JavaScript代码创建代码来验证功能的正确性。程序员这样做可以确保他们的程序按预期运行，并且在未来的更新中不会引入新的错误。

# 如何操作：
我们将使用 Jest，一个广泛用于JavaScript的测试框架。首先安装Jest：

```bash
npm install --save-dev jest
```

然后，编写一个简单函数和对应的测试用例：

```javascript
// math.js
function sum(a, b) {
  return a + b;
}
module.exports = sum;
```

```javascript
// math.test.js
const sum = require('./math');

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

运行测试：

```bash
npx jest
```

输出应该显示测试通过：

```
PASS  ./math.test.js
✓ adds 1 + 2 to equal 3 (5ms)
```

# 深入探讨
测试是软件开发的老问题。在JavaScript中，有许多不同的测试框架，如 Mocha, Jasmine, 和 Ava，Jest是最新的，提供了快速和沙箱隔离功能。它允许Mock对象和依赖，非常适合大型代码库。

Jest采用"零配置"策略，尽管它可配置性很强。其他测试框架可能需要更多的设置工作。在选择测试框架时，你应该考虑你的项目需求，以及框架的社区支持和维护情况。

# 参见
- Jest官方文档: [https://jestjs.io/docs/getting-started](https://jestjs.io/docs/getting-started)
- 测试驱动开发(TDD)介绍: [https://www.agilealliance.org/glossary/tdd/](https://www.agilealliance.org/glossary/tdd/)
- JavaScript测试框架比较: [https://stateofjs.com/](https://stateofjs.com/)
