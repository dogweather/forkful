---
date: 2024-01-26 00:54:05.055833-07:00
description: "\u30A8\u30E9\u30FC\u30CF\u30F3\u30C9\u30EA\u30F3\u30B0\u3068\u306F\u3001\
  \u30B3\u30FC\u30C9\u3067\u4F55\u304B\u304C\u3046\u307E\u304F\u3044\u304B\u306A\u3044\
  \u6642\u306E\u7BA1\u7406\u65B9\u6CD5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u304C\u305F\u3060\u30AF\u30E9\u30C3\u30B7\u30E5\u3059\u308B\u306E\u3067\u306F\u306A\
  \u304F\u3001\u3046\u307E\u304F\u5931\u6557\u3057\u3001\u30E6\u30FC\u30B6\u30FC\u306B\
  \u660E\u78BA\u306A\u6307\u793A\u3092\u51FA\u3059\u305F\u3081\u306B\u91CD\u8981\u3067\
  \u3059\u3002"
lastmod: '2024-03-13T22:44:42.687337-06:00'
model: gpt-4-1106-preview
summary: "\u30A8\u30E9\u30FC\u30CF\u30F3\u30C9\u30EA\u30F3\u30B0\u3068\u306F\u3001\
  \u30B3\u30FC\u30C9\u3067\u4F55\u304B\u304C\u3046\u307E\u304F\u3044\u304B\u306A\u3044\
  \u6642\u306E\u7BA1\u7406\u65B9\u6CD5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u304C\u305F\u3060\u30AF\u30E9\u30C3\u30B7\u30E5\u3059\u308B\u306E\u3067\u306F\u306A\
  \u304F\u3001\u3046\u307E\u304F\u5931\u6557\u3057\u3001\u30E6\u30FC\u30B6\u30FC\u306B\
  \u660E\u78BA\u306A\u6307\u793A\u3092\u51FA\u3059\u305F\u3081\u306B\u91CD\u8981\u3067\
  \u3059\u3002"
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

## 何となぜ？

エラーハンドリングとは、コードで何かがうまくいかない時の管理方法です。プログラムがただクラッシュするのではなく、うまく失敗し、ユーザーに明確な指示を出すために重要です。

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
