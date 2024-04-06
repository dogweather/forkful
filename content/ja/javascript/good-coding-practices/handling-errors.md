---
date: 2024-01-26 00:54:05.055833-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A JavaScript\u306E\u30A8\u30E9\u30FC\
  \u30CF\u30F3\u30C9\u30EA\u30F3\u30B0\u306F\u9032\u5316\u3057\u3066\u3044\u307E\u3059\
  \u3002\u6614\uFF08ES3, 1999\u5E74\u9803\uFF09\u306B\u306F`try-catch`\u30D6\u30ED\
  \u30C3\u30AF\u3060\u3051\u3067\u3057\u305F\u3002\u3042\u307E\u308A\u67D4\u8EDF\u3067\
  \u306F\u3042\u308A\u307E\u305B\u3093\u3067\u3057\u305F\u304C\u3001\u4ED5\u4E8B\u306F\
  \u3067\u304D\u307E\u3057\u305F\u3002 ES6\uFF082015\u5E74\uFF09\u304C\u30D7\u30ED\
  \u30DF\u30B9\u3092\u5C0E\u5165\u3057\u3001\u975E\u540C\u671F\u30A8\u30E9\u30FC\u3092\
  \u3088\u308A\u4E0A\u54C1\u306B\u6271\u3046 `.then()` \u3068\u2026"
lastmod: '2024-04-05T22:50:56.552873-06:00'
model: gpt-4-1106-preview
summary: "\u3069\u3046\u3084\u3063\u3066\uFF1A JavaScript\u306E\u30A8\u30E9\u30FC\u30CF\
  \u30F3\u30C9\u30EA\u30F3\u30B0\u306F\u9032\u5316\u3057\u3066\u3044\u307E\u3059\u3002\
  \u6614\uFF08ES3, 1999\u5E74\u9803\uFF09\u306B\u306F`try-catch`\u30D6\u30ED\u30C3\
  \u30AF\u3060\u3051\u3067\u3057\u305F\u3002\u3042\u307E\u308A\u67D4\u8EDF\u3067\u306F\
  \u3042\u308A\u307E\u305B\u3093\u3067\u3057\u305F\u304C\u3001\u4ED5\u4E8B\u306F\u3067\
  \u304D\u307E\u3057\u305F\u3002 ES6\uFF082015\u5E74\uFF09\u304C\u30D7\u30ED\u30DF\
  \u30B9\u3092\u5C0E\u5165\u3057\u3001\u975E\u540C\u671F\u30A8\u30E9\u30FC\u3092\u3088\
  \u308A\u4E0A\u54C1\u306B\u6271\u3046 `.then()` \u3068 `.catch()` \u3092\u79C1\u305F\
  \u3061\u306B\u4E0E\u3048\u307E\u3057\u305F\u3002"
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
