---
title:    "TypeScript: 「現在の日付を取得する」"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

《なぜ》
現在の日付を取得する理由を説明します。

## なぜ

プログラマーにとって、現在の日付を取得することは非常に重要です。時には、タイムスタンプや有効期限などの特定の日付情報を取得する必要があります。また、特定の日付に応じて条件分岐する必要がある場合もあります。つまり、現在の日付を取得することは、日付に関する多くのプログラミング上のタスクに必要不可欠な作業です。

## ハウツー

```TypeScript
// 現在の日付を取得する
const currentDate = new Date();

// フォーマットを指定して日付を取得する
const formattedDate = currentDate.toLocaleDateString('ja-JP', {year: 'numeric', month: 'long', day: 'numeric'});
console.log(formattedDate); // 2021年9月1日

// タイムスタンプを取得する
const timestamp = currentDate.getTime();
console.log(timestamp); // 1630431600000 (2021年9月1日を表すタイムスタンプ)

// 特定の日付に応じて条件分岐する例
if (currentDate.getMonth() === 11) {
    console.log("今年はクリスマスです！");
} else {
    console.log("クリスマスまでまだ時間があります。");
}
```

コードの出力:

```
2021年9月1日
1630431600000
クリスマスまでまだ時間があります。
```

## ディープダイブ

現在の日付を取得する方法は様々ありますが、主に3つの方法があります。

1つ目は、`new Date()`を使って現在の日付を取得する方法です。この方法は最も簡単で、新しいDateオブジェクトを作成して現在の日付を取得できます。

2つ目は、`Date.prototype.toLocaleDateString()`を使ってフォーマットを指定して日付を取得する方法です。この方法を使うと、日付を任意のフォーマットに変換できます。

3つ目は、`Date.prototype.getTime()`を使ってタイムスタンプを取得する方法です。タイムスタンプは日付をミリ秒単位で表したもので、プログラミング言語やデータベースで日付を取り扱う際に便利です。

## 関連リンク

- [MDN Web Docs: Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Language Specification: Date](https://github.com/microsoft/TypeScript/blob/master/doc/spec.md#14-built-in-operators)
- [W3Schools: JavaScript Date Objects](https://www.w3schools.com/js/js_dates.asp)