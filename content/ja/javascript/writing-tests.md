---
title:                "テストの作成"
date:                  2024-01-19
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (テストを書くとは？ & なぜ？)
JavaScriptでテストを書くとは、コードが期待通りに動くか検証する過程です。これにより、バグの発見、リファクタリングの容易さ、そして将来的な機能追加に対する信頼性が高まります。

## How to: (方法)
以下はJestを使ったテストの例です。このライブラリはシンプルで人気があるため、紹介します。

```javascript
// sum.js
function sum(a, b) {
  return a + b;
}
module.exports = sum;

// sum.test.js
const sum = require('./sum');

test('1 + 2 は 3 になる', () => {
  expect(sum(1, 2)).toBe(3);
});

// コマンドラインでテストを実行
// $ jest
```

出力例：
```
PASS  ./sum.test.js
✓ 1 + 2 は 3 になる (5ms)
```

## Deep Dive (深掘り)
テストはソフトウェア開発の古典的な部分であり、TDD（テスト駆動開発）のようなアプローチがあります。Jestは多くの代替品の中から選ばれることが多く、そのインターフェースの単純さと設定のしやすさからはじまり、リアクティブなウォッチモードやスナップショットテストなどの機能が魅力です。他にも、Mocha、Jasmine、QUnit等が知られています。

## See Also (関連情報)
- Jest公式ドキュメント: https://jestjs.io/ja/
- Jasmine: https://jasmine.github.io/
- Mocha: https://mochajs.org/
- QUnit: https://qunitjs.com/

Jest以外にもいくつかのテストフレームワークが存在するので、プロジェクトの要件に応じて最適なものを選択しましょう。
