---
title:                "テストの書き方"
html_title:           "Javascript: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## なぜ

テストを書くことは、コードを書く上で重要なプロセスです。テストは、コードの品質を保証し、バグを早期に発見することに役立ちます。

## 作り方

テストを書くためには、まずテスト用のライブラリをインストールする必要があります。例えば、JestやMochaなどが良い選択肢です。次に、テスト用のファイルを作成し、テストしたい機能を書き、期待する結果を示します。以下は、Jestを使った例です。

```Javascript
// calculator.js
function add(x, y) {
    return x + y;
}

// calculator.test.js
const add = require('./calculator');

test('adds 1 + 2 to equal 3', () => {
    expect(add(1, 2)).toBe(3);
});
```

上記の例では、calculator.jsで定義されたadd関数をJestを使ってテストしています。`test('機能の説明', () => {})`の形式でテストを書き、`expect(関数名(引数)).toBe(期待する結果)`で期待する結果を示します。テストを実行するには、`npm test`コマンドを実行します。

## ディープダイブ

テストを書く際には、必ずカバレッジ率を確認しましょう。カバレッジ率とは、テストを通過したコードの割合を示すものです。カバレッジ率が高いほど、テストの信頼性が高くなります。また、コードカバレッジツールを使うことで、テストを網羅的に書くことができます。

## さらに参考になるリンク

- [Jest documentation](https://jestjs.io/docs/en/getting-started)
- [Mocha documentation](https://mochajs.org/)
- [Code coverage tools](https://babeljs.io/docs/en/next/babel-plugin-istanbul.html)