---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:27.794387-07:00
description: "\u65B9\u6CD5: Jest\u306F\u3001JavaScript\u3067\u30E6\u30CB\u30C3\u30C8\
  \u30C6\u30B9\u30C8\u3092\u66F8\u304F\u305F\u3081\u306E\u30D5\u30EC\u30F3\u30C9\u30EA\
  \u30FC\u306AAPI\u3092\u63D0\u4F9B\u3059\u308B\u4EBA\u6C17\u306E\u30C6\u30B9\u30C8\
  \u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u3067\u3059\u3002\u6700\u5C0F\u9650\u306E\
  \u8A2D\u5B9A\u304C\u5FC5\u8981\u3067\u3001\u30E2\u30C3\u30AF\u95A2\u6570\u3001\u30BF\
  \u30A4\u30DE\u30FC\u3001\u30B9\u30CA\u30C3\u30D7\u30B7\u30E7\u30C3\u30C8\u30C6\u30B9\
  \u30C8\u306A\u3069\u306E\u6A5F\u80FD\u304C\u4ED8\u5C5E\u3057\u3066\u3044\u307E\u3059\
  \u3002 1. **\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB**."
lastmod: '2024-03-13T22:44:42.682416-06:00'
model: gpt-4-0125-preview
summary: "Jest\u306F\u3001JavaScript\u3067\u30E6\u30CB\u30C3\u30C8\u30C6\u30B9\u30C8\
  \u3092\u66F8\u304F\u305F\u3081\u306E\u30D5\u30EC\u30F3\u30C9\u30EA\u30FC\u306AAPI\u3092\
  \u63D0\u4F9B\u3059\u308B\u4EBA\u6C17\u306E\u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\
  \u30EF\u30FC\u30AF\u3067\u3059\u3002\u6700\u5C0F\u9650\u306E\u8A2D\u5B9A\u304C\u5FC5\
  \u8981\u3067\u3001\u30E2\u30C3\u30AF\u95A2\u6570\u3001\u30BF\u30A4\u30DE\u30FC\u3001\
  \u30B9\u30CA\u30C3\u30D7\u30B7\u30E7\u30C3\u30C8\u30C6\u30B9\u30C8\u306A\u3069\u306E\
  \u6A5F\u80FD\u304C\u4ED8\u5C5E\u3057\u3066\u3044\u307E\u3059."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

## 方法:


### ネイティブアプローチ (Jestを使用)
Jestは、JavaScriptでユニットテストを書くためのフレンドリーなAPIを提供する人気のテストフレームワークです。最小限の設定が必要で、モック関数、タイマー、スナップショットテストなどの機能が付属しています。

1. **インストール**:

```bash
npm install --save-dev jest
```

2. **シンプルなテストを書く**:

`sum.test.js`というファイルを作成します：

```javascript
const sum = require('./sum'); // この関数は単純に2つの数字を足すと仮定

test('1 + 2 が 3 になる', () => {
  expect(sum(1, 2)).toBe(3);
});
```

3. **テストを実行する**:

```bash
npx jest
```

**サンプル出力:**

```plaintext
PASS  ./sum.test.js
✓ 1 + 2 が 3 になる (5ms)
```

### 非同期コードのテスト
Jestは、プロミスとasync/await構文をテストするのを簡単にします：

```javascript
// asyncSum.js
async function asyncSum(a, b) {
  return Promise.resolve(a + b);
}

// asyncSum.test.js
test('async 加算が機能する', async () => {
  await expect(asyncSum(1, 2)).resolves.toBe(3);
});

```

### サードパーティライブラリーの使用 (Mocha & Chai)
Mochaは、より表現力豊かなテストのためにアサーションライブラリーのChaiとしばしば使用される、別の人気のあるテストフレームワークです。

1. **インストール**:

```bash
npm install --save-dev mocha chai
```

2. **MochaとChaiでテストを書く**:

`calculate.test.js`を作成します：

```javascript
const chai = require('chai');
const expect = chai.expect;

const calculate = require('./calculate'); // シンプルな計算モジュール

describe('Calculate', function() {
  it('should sum two values', function() {
    expect(calculate.sum(5, 2)).to.equal(7);
  });
});
```

3. **Mochaでテストを実行する**:

`package.json`にスクリプトを追加します：

```json
"scripts": {
  "test": "mocha"
}
```

そして実行します：

```bash
npm test
```

**サンプル出力:**

```plaintext
  Calculate
    ✓ should sum two values


  1 passing (8ms)
```

これらの例は、JavaScriptでの基本的なテストの書き方と実行方法を示しています。JestやMochaとChaiのようなテストフレームワークを採用することは、アプリケーションテストのための堅牢な基盤を提供し、アップデートやリファクタリングを通じてコードが意図した通りに機能することを確実にするのに役立ちます。
