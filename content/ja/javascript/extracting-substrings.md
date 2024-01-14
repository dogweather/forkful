---
title:                "Javascript: 部分文字列を抽出する"
simple_title:         "部分文字列を抽出する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の一部を抽出することは、プログラミングにおいて非常に便利です。例えば、あなたが文書を解析したり、特定の単語を検索したりする場合、文字列から必要な情報を抜き出す必要があります。そんなときに、このsubstring(部分文字列)の機能があなたを助けてくれます。

## ハウツー

### substring() メソッドの使い方

最初に、文字列から一部を抽出する基本的な方法をご紹介します。substring()メソッドを使うと、指定した位置から指定した長さの部分文字列を返すことができます。以下のコードをご覧ください。

```Javascript
var str = "これは素晴らしいブログです！";
var part = str.substring(2, 8);

console.log(part); // => 素晴らしい
```

上記の例では、strから2番目（3文字目）から8番目（9文字目）までの部分文字列が抽出されています。また、インデックス番号は0から始まることに注意してください。

### indexOf() メソッドと組み合わせて使う

次に、substring()メソッドをより便利に使う方法をご紹介します。indexOf()メソッドと組み合わせることで、指定した文字列が最初に登場する位置から部分文字列を抽出することができます。以下のコードをご覧ください。

```Javascript
var str = "こんにちは、私はSmithです！";
var part = str.substring(str.indexOf("、") + 1, str.indexOf("です"));

console.log(part); // => 私はSmith
```

上記の例では、strから「、」の次の位置から「です」の位置までの部分文字列が抽出されています。indexOf()メソッドは指定した文字列の位置を返すので、substring()メソッドで使うことで指定した文字列を基準に部分文字列を抽出することができます。

## ディープダイブ

substring()メソッドは非常に便利ですが、注意しなければならない点もあります。例えば、最初に指定した位置よりも大きい値を指定した場合、自動的に先頭から部分文字列を取得しようとします。また、負の値を指定した場合、自動的に文字列の後ろから部分文字列を取得しようとします。これらのケースはプログラムの実行時にエラーが発生する可能性がありますので、注意してください。

## 参考リンク

- [substring() メソッドのドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [indexOf() メソッドのドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/String/indexOf)
- [JavaScript 文字列の操作方法のまとめ](https://techplay.jp/column/290)

## また見る

[抽出（エクストラクション） - Wikipedia](https://ja.wikipedia.org/wiki/%E6%8A%BD%E5%87%BA_(%E8%A8%98%E5%8F%B7%E6%B3%95))