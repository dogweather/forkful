---
title:    "Javascript: 文字列の長さを見つける"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## なぜ
趣味として、または仕事上のプロジェクトで、文字列の長さを知る必要がある場合があります。これにより、文字列を適切に処理し、必要に応じて修正することができます。

## 使い方
文字列の長さを取得するには、`length`メソッドを使用します。以下のコード例をご覧ください。

```Javascript
let str = "こんにちは、世界！";
console.log(str.length);
```

上記のコードの出力結果は、`13`となります。これは、`str`変数に格納された文字列全体の長さを示しています。また、特定の文字だけを抽出し、その文字の数を知りたい場合は、2つの方法があります。

1. `charAt()`メソッドを使用する方法：

```Javascript
let str = "こんにちは、世界！";
console.log(str.charAt(1).length); // "に"の文字数である`1`が表示されます
```

2. `substring()`メソッドを使用する方法：

```Javascript
let str = "こんにちは、世界！";
console.log(str.substring(2, 5).length); // "に"の文字数である`3`が表示されます
```

いずれの方法でも、指定した文字の数が返されることがわかります。

## 詳細
文字列の長さを取得するとはどういうことでしょうか。実際には、文字列は文字の配列として扱われます。つまり、文字列内の各文字は、インデックス番号によってアクセスできるように番号付けされています。例えば、最初の文字は`[0]`、次の文字は`[1]`というように続きます。`length`メソッドを使用することで、文字列の総文字数を数えることができ、それぞれの文字の位置を特定することができます。

## 参考リンク
- [JavaScript の文字列を制限文字数に短縮する方法](https://techacademy.jp/magazine/19991)
- [JavaScript substring() メソッド](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [文字列の長さを取得する方法](https://uxmilk.jp/63290)