---
title:    "Javascript: テキストの検索と置換"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ検索と置換を行う必要があるのか？

テキストをプログラムで操作する際、特定のキーワードや文字列を一括で置換することは非常に便利です。例えば、大量のファイル内の特定の単語を修正する際や、古いコードを新しいコードに置き換える際に役立ちます。この記事では、Javascriptで簡単に検索と置換を行う方法をご紹介します。

## 方法

検索と置換を行うためには、Javascriptで提供されている`replace()`メソッドを使用します。このメソッドは、最初の引数に検索対象の文字列、2つ目の引数に置換する文字列を指定します。例えば、以下のように使用することができます。

```Javascript
// 文字列の置換
let str = "Hello world!";
let newStr = str.replace("world", "JavaScript");
console.log(newStr); // 出力結果: Hello JavaScript!

// 正規表現を使用した置換
let text = "今日はとても暑いです。明日は晴れるかな？";
let newText = text.replace(/暑い/, "寒い").replace(/晴れる/, "雨が降る");
console.log(newText); // 出力結果: 今日はとても寒いです。明日は雨が降るかな？
```

## 深堀り情報

`replace()`メソッドは単一の文字列だけでなく、正規表現を使用してマッチする複数の文字列を一括で置換することもできます。また、第2引数には、置換する文字列だけでなく、コールバック関数を渡すこともできます。これは、検索文字列にマッチした箇所を自由に操作することができるため、より高度な置換操作が可能になります。

```Javascript
// コールバック関数を使用した置換
let text = "今日はとても暑いです。明日は晴れるかな？";
let newText = text.replace(/暑い/, function(match){ // マッチした文字列を受け取る
  return match.toUpperCase().replace("暑い", "寒い");
});
console.log(newText); // 出力結果: 今日はとても寒いです。明日は晴れるかな？
```

`replace()`メソッドは、大文字と小文字を区別するため、厳密なマッチングを行います。しかし、場合によっては大文字と小文字を区別しない置換が必要な場合もあります。そのような場合は正規表現のフラグを指定することで、大文字と小文字を区別しない置換が可能になります。

## 参考リンク

- [MDN Web Docs: replace()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions)
- [正規表現クックブック](https://www amazon.co.jp/正規表現クックブック-ギデオン・スタール/dp/4873113690)

### 他の記事を見る

- [Javascriptで正規表現を使用したテキスト操作の方法](https://www.example.com/ja/javascript-regular-expressions)
- [Javascriptでの文字列操作に必須の知識を学ぶ](https://www.example.com/ja/javascript-string-basics)