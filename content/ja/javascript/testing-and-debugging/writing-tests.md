---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:27.794387-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.682416-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\u306F\u3001\
  \u30B3\u30FC\u30C9\u304C\u671F\u5F85\u901A\u308A\u306B\u52D5\u4F5C\u3059\u308B\u3053\
  \u3068\u3092\u4FDD\u8A3C\u3059\u308B\u305F\u3081\u306B\u81EA\u52D5\u30B9\u30AF\u30EA\
  \u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u5B9F\u8DF5\u3092\u6307\u3057\u307E\u3059\
  \u3002\u3053\u308C\u306B\u3088\u308A\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u306E\u4FE1\u983C\u6027\u3068\u4FDD\u5B88\u6027\u304C\u5927\u5E45\u306B\u5411\
  \u4E0A\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u65E9\
  \u671F\u306B\u30D0\u30B0\u3092\u6355\u6349\u3057\u3001\u30B3\u30FC\u30C9\u306E\u30EA\
  \u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3057\u3001\u65B0\
  \u6A5F\u80FD\u304C\u65E2\u5B58\u306E\u6A5F\u80FD\u3092\u58CA\u3055\u306A\u3044\u3053\
  \u3068\u3092\u78BA\u8A8D\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

## 何となぜ？

JavaScriptでテストを書くことは、コードが期待通りに動作することを保証するために自動スクリプトを作成する実践を指します。これにより、アプリケーションの信頼性と保守性が大幅に向上します。プログラマーは、早期にバグを捕捉し、コードのリファクタリングを容易にし、新機能が既存の機能を壊さないことを確認するためにこれを行います。

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
