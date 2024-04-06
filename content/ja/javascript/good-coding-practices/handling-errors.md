---
date: 2024-01-26 00:54:05.055833-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A \u3053\u3053\u306B\u53E4\u5178\u7684\
  \u306A `try-catch` \u30D6\u30ED\u30C3\u30AF\u304C\u3042\u308A\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:43.473572-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

## どうやって：
ここに古典的な `try-catch` ブロックがあります。

```javascript
try {
  // エラーを投げる可能性のあるコード
  let result = potentiallyRiskyOperation();
  console.log('成功:', result);
} catch (error) {
  // エラーが投げられた場合の対応
  console.error('おっと:', error.message);
}
```

エラーが発生しない場合のサンプル出力：
```
成功: 42
```

エラーがある場合：
```
おっと: 何かがうまくいかなかった
```

プロミスが関与する非同期コードでは、`async` 関数の中で `try-catch` を使用します：

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('データを取得:', data);
  } catch (error) {
    console.error('データ取得エラー:', error.message);
  }
}

fetchData();
```

## 深掘り
JavaScriptのエラーハンドリングは進化しています。昔（ES3, 1999年頃）には`try-catch`ブロックだけでした。あまり柔軟ではありませんでしたが、仕事はできました。

ES6（2015年）がプロミスを導入し、非同期エラーをより上品に扱う `.then()` と `.catch()` を私たちに与えました。

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('データを取得:', data))
  .catch(error => console.error('データ取得エラー:', error.message));
```

実装の詳細について言えば、エラーが投げられると、JavaScriptエンジンは`message`や`stack`などの有用なプロパティを持つ`Error`オブジェクトを作成します。複雑なアプリで便利なのは、`Error`クラスを拡張してカスタムエラータイプを作成することもできます。

代替案？エラーハンドリングを無視する（悪いアイデア）、エラー優先のパラメータでコールバックを使用する（こんにちは、Node.jsスタイル）、または自分たちのテイクを提供するライブラリやフレームワークを使って更に洗練されたことをする、などがあります。

## 参照
エラーハンドリングについての詳細は：

- MDNのtry-catchについて： [MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Awaitについて： [MDN async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- プロミスに関するガイド： [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- カスタムエラーの作成と投げる： [MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
