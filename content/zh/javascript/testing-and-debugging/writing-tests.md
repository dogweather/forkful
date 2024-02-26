---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:16.605745-07:00
description: "\u5728JavaScript\u4E2D\u7F16\u5199\u6D4B\u8BD5\u6307\u7684\u662F\u521B\
  \u5EFA\u81EA\u52A8\u5316\u811A\u672C\u6765\u8FD0\u884C\u4F60\u7684\u4EE3\u7801\uFF0C\
  \u4EE5\u786E\u4FDD\u5B83\u7684\u884C\u4E3A\u7B26\u5408\u9884\u671F\uFF0C\u8FD9\u53EF\
  \u4EE5\u663E\u8457\u63D0\u9AD8\u4F60\u7684\u5E94\u7528\u7A0B\u5E8F\u7684\u53EF\u9760\
  \u6027\u548C\u53EF\u7EF4\u62A4\u6027\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u5C3D\u65E9\u6355\u6349\u5230\u9519\u8BEF\uFF0C\u4FBF\u4E8E\u4EE3\u7801\
  \u91CD\u6784\uFF0C\u4EE5\u53CA\u786E\u4FDD\u65B0\u529F\u80FD\u4E0D\u4F1A\u7834\u574F\
  \u73B0\u6709\u529F\u80FD\u3002"
lastmod: '2024-02-25T18:49:45.776933-07:00'
model: gpt-4-0125-preview
summary: "\u5728JavaScript\u4E2D\u7F16\u5199\u6D4B\u8BD5\u6307\u7684\u662F\u521B\u5EFA\
  \u81EA\u52A8\u5316\u811A\u672C\u6765\u8FD0\u884C\u4F60\u7684\u4EE3\u7801\uFF0C\u4EE5\
  \u786E\u4FDD\u5B83\u7684\u884C\u4E3A\u7B26\u5408\u9884\u671F\uFF0C\u8FD9\u53EF\u4EE5\
  \u663E\u8457\u63D0\u9AD8\u4F60\u7684\u5E94\u7528\u7A0B\u5E8F\u7684\u53EF\u9760\u6027\
  \u548C\u53EF\u7EF4\u62A4\u6027\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u5C3D\u65E9\u6355\u6349\u5230\u9519\u8BEF\uFF0C\u4FBF\u4E8E\u4EE3\u7801\u91CD\
  \u6784\uFF0C\u4EE5\u53CA\u786E\u4FDD\u65B0\u529F\u80FD\u4E0D\u4F1A\u7834\u574F\u73B0\
  \u6709\u529F\u80FD\u3002"
title: "\u7F16\u5199\u6D4B\u8BD5"
---

{{< edit_this_page >}}

## 什么和为什么？

在JavaScript中编写测试指的是创建自动化脚本来运行你的代码，以确保它的行为符合预期，这可以显著提高你的应用程序的可靠性和可维护性。程序员这样做是为了尽早捕捉到错误，便于代码重构，以及确保新功能不会破坏现有功能。

## 如何操作：

### 原生方法（使用Jest）

Jest是一个流行的测试框架，为编写JavaScript单元测试提供了友好的API。它需要最小的配置，并带有诸如mock函数、计时器和快照测试等功能。

1. **安装**：

```bash
npm install --save-dev jest
```

2. **编写一个简单的测试**：

创建一个名为`sum.test.js`的文件：

```javascript
const sum = require('./sum'); // 假设这个函数简单地加两个数字

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **运行你的测试**：

```bash
npx jest
```

**示例输出：**

```plaintext
PASS  ./sum.test.js
✓ adds 1 + 2 to equal 3 (5ms)
```

### 测试异步代码

Jest使得测试承诺和async/await语法变得简单：

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('async addition works', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### 使用第三方库（Mocha与Chai）

Mocha是另一个流行的测试框架，常与断言库Chai结合使用，以便进行更具表达性的测试。

1. **安装**：

```bash
npm install --save-dev mocha chai
```

2. **使用Mocha和Chai编写测试**：

创建`calculate.test.js`：

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // 一个简单的计算模块

describe('Calculate', function() {
  it('should sum two values', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **使用Mocha运行你的测试**：

在你的`package.json`中添加一个脚本：

```json
"scripts": {
  "test": "mocha"
}
```

然后执行：

```bash
npm test
```

**示例输出：**

```plaintext
  Calculate
    ✓ should sum two values


  1 passing (8ms)
```

这些示例说明了在JavaScript中编写和执行基本测试的方法。采用像Jest或Mocha与Chai这样的测试框架可以为健壮的应用程序测试提供坚实的基础，帮助确保你的代码在更新和重构过程中按预期运行。
