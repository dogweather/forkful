---
title:    "Ruby: パターンに一致する文字を削除する"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使ってパターンにマッチする文字を削除することについて、なぜ誰かが取り組むのかを説明します。

## 方法

```Ruby
string = "123abc456def789ghi"
new_string = string.gsub(/[a-z]/, "")
puts new_string
```

出力：123456789

正規表現の`/[a-z]/`は、文字列からaからzまでの文字をマッチさせます。`gsub`メソッドを使って、そのマッチした文字を空の文字列に置き換えることで、文字列からマッチした文字を削除することができます。

## 深堀り

正規表現についてもっと詳しく学びたい方は、Rubyの公式ドキュメントやオンラインのチュートリアルを参考にしてください。

## その他参考リンク

[正規表現チュートリアル: Ruby版](https://www.ruby-lang.org/ja/documentation/quickstart/2/)
[正規表現メタキャラクタ](https://docs.ruby-lang.org/ja/latest/doc/spec=2fregexp.html#metachar)
[正規表現を学習するためのオンラインコース](https://www.coursera.org/courses?query=ruby%20regular%20expressions)