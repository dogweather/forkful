---
title:                "PHP: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ検索と置換テキストを行うのか

テキストを書くとき、時々誤字や変更が必要になることがあります。それを手動で一つずつ修正するのは非常に面倒です。そのため、PHPプログラミングで検索と置換を行うことができます。

## 方法

テキストを検索して、特定のテキストを置換するには、以下のようなコードを使用します。

```PHP
$string = "私の名前は田中です。";
$search = "田中";
$replace = "山田";
echo str_replace($search, $replace, $string);
```
このコードを実行すると、出力は以下のようになります。

私の名前は山田です。

このように、 `str_replace()` 関数を使用して、簡単にテキストの検索と置換を行うことができます。

## 深堀り

しかし、上記の例では単純な検索と置換を行っています。実際には、検索結果を配列として取得する `preg_replace()` 関数を使用することもできます。また、正規表現を使用してパターンマッチングを行うこともできます。

さらに、テキストの置換だけでなく、テキストの一部を抽出することもできます。 `preg_match()` 関数を使うと、指定したパターンに一致した文字列を取得することができます。

## はてな

詳しい情報やさらなる応用例については、PHPの公式ドキュメントを参照することをお勧めします。また、正規表現についてはオンラインのチュートリアルやコーディングサイトで学ぶことができます。

## 関連リンク

- [PHP公式ドキュメント](https://www.php.net/manual/ja/)
- [正規表現チュートリアル](https://regexone.com/)
- [コーディングサイト「コードアカデミー」](https://www.codecademy.com/)