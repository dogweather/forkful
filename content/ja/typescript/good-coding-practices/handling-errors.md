---
title:                "エラー処理"
aliases: - /ja/typescript/handling-errors.md
date:                  2024-01-26T00:58:41.341745-07:00
model:                 gpt-4-1106-preview
simple_title:         "エラー処理"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/handling-errors.md"
---

{{< edit_this_page >}}

## 何となぜ？
エラー処理とは、予期せぬことを予測することです。コードで問題が発生した際に、それをどう管理するかということです。クラッシュを避け、予期せぬことが発生したとしてもユーザーにスムーズな経験を提供するために行います。

## 方法：
TypeScriptでは、エラー処理には多くの場合`try`、`catch`、`finally`ブロックが関わってきます。

```typescript
function riskyOperation() {
  throw new Error("何か問題が発生しました！");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("エラーをキャッチしました：", error.message);
  } finally {
    console.log("これは常に実行されます、エラーがあろうとなかろうと。");
  }
}

handleErrors();
```

サンプル出力：

```
エラーをキャッチしました：何か問題が発生しました！
これは常に実行されます、エラーがあろうとなかろうと。
```

プロミスを用いた非同期の例：

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // エラーをシミュレート
    reject("ひどく失敗しました");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("非同期エラーをキャッチしました：", error);
  }
}

handleAsyncErrors();
```

サンプル出力：

```
非同期エラーをキャッチしました：ひどく失敗しました
```

## 深堀り
エラー処理はプログラミングの礎石であり、始まりから存在しています。JavaScriptを基にしたTypeScriptでは、ECMAScript 2017でのasync/awaitの導入により、エラー処理がより堅牢になりました。それ以前は、非同期コード内のエラーを処理するためにコールバック関数やプロミスに頼ることが多かったです。

TypeScriptで`try/catch`の代わりに使用できる代替手段として、Reactのようなフレームワークによるエラーバウンダリがあります。サーバー側での処理には、Express.jsのようなプラットフォームでミドルウェアを使用してエラー管理を一元化することができます。

実装上では、TypeScriptには独自のエラー処理機構はなく、JavaScriptのものに依存しています。カスタムエラークラスは`Error`クラスを拡張して、より記述的なエラー情報を提供することができます。

## 関連項目
- [MDNのtry/catchについて](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [MDNのAsync/Awaitについて](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Reactにおけるエラーバウンダリの使用](https://reactjs.org/docs/error-boundaries.html)
- [Express.jsのエラー処理](https://expressjs.com/en/guide/error-handling.html)
