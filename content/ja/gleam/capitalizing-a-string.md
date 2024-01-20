---
title:                "文字列を大文字にする"
html_title:           "Gleam: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を大文字化するとは、文字列内のすべての文字を大文字に変換することを指します。これは、プログラム内で一貫性を維持するため、またはユーザ入力の正規化を行うためによく行われます。

## 使い方：

Gleamでは、`to_upper`関数を使用して文字列を大文字に変換できます。例は以下のとおりです。

```gleam
import gleam/string

string.to_upper("こんにちは、Gleamの世界！")
// 出力: "こんにちは、gleAMの世界！"
```
このコードは、主要な文字列の各文字を大文字に変換します。

## ディープダイブ：

大文字化は遠い昔から存在する技術で、コンピュータが登場する前から行われていました。大文字と小文字の概念自体が存在しない言語に対する考慮が必要であり、より新しいUnicode対応関数を使用することが推奨されます。

Gleamの`to_upper`関数は、Unicode文字列を正確に大文字化します。つまり、ASCIIだけでなく、非ASCII文字もサポートします。ただし、あらゆる言語、特に大文字・小文字の区別しない言語の全てのエッジケースを正確に考慮しているわけではありません。

大文字化には代替方法も多く存在します。独自のロジックを作成して大文字化することも可能ですが、通常は標準ライブラリの関数を使用することが推奨されます。独自の方法では、特殊なエッジケースを処理するのが難しく、時間も労力も消費します。

## ほかの情報：

以下に、Gleamと関連する追加資料へのリンクをいくつか提供します。

- Gleam公式文書：https://gleam.run/docs/
- Gleamの文字列操作に関する詳細：https://gleam.run/stdlib/string.html
- ASCIIとUnicodeについての詳細：https://en.wikipedia.org/wiki/ASCII, https://en.wikipedia.org/wiki/Unicode