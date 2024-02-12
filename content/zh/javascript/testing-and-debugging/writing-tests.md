---
title:                "编写测试"
aliases:
- /zh/javascript/writing-tests.md
date:                  2024-02-03T19:31:16.605745-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
