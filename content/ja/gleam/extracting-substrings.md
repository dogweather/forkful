---
title:                "サブストリングの抽出"
html_title:           "Gleam: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

＃＃ なぜ
サブストリングを抽出することに取り組む理由を最大2文で説明します。

## ハウツー
サブストリングを抽出する方法の例と、 ```Gleam ...``` コードブロック内のサンプル出力を示します。
```
// 簡単な文字列
let string = "こんにちは、Gleam読者のみなさん！";

// 2番目から5番目までの文字を抽出
let substring = string.slice(1, 5);

// 出力："んにちは"
```
```
// 複雑な文字列
let sentence = "Basho's most famous haiku is \"古池や蛙飛び込む水の音\".";

// 最初の12文字を抽出
let haiku = sentence.slice(0, 12);

// 出力："Basho's most"
```

## 深堀り
サブストリングとは、既存の文字列から一部の文字を抜き出すことを指します。これは、文字列を処理する上で頻繁に使用されるテクニックであり、例えば特定の単語を含むかどうかを確認する場合などに便利です。

## 参考
「Gleamの基礎：文字列操作」<リンク>
「Gleam公式ドキュメント：サブストリングを抽出する」<リンク>