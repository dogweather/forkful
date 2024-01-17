---
title:                "正規表現の使用"
html_title:           "Fish Shell: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なに？なんで？
正規表現を使用することは、文字列のパターンをマッチングするプロセスを指します。プログラマーは、データの抽出や検索、置換などの作業をより高速かつ効率的に行うために正規表現を使用します。

## 使い方：
```Fish Shell```コードブロック内でコーディングの例とサンプル出力を示します。

```fish
# パターンにマッチするテキストを抽出する
set text "こんにちは、私はJamesです。"
echo $text | grep -o 'James'
```
出力：
```
James
```

```fish
# 文字列を置換する
set text "私はプログラマーです。"
echo $text | sed -e 's/プログラマー/デザイナー/'
```
出力：
```
私はデザイナーです。
```

```fish
# パターンが存在するかを確認する
set text "私のメールアドレスはtest@gmail.comです。"
if string match --regex ".*@gmail\.com" $text
    echo "メールアドレスが存在します。"
end
```
出力：
```
メールアドレスが存在します。
```

## ディープダイブ：
正規表現は、1960年代に開発された形式言語であり、データ処理に使用されてきました。他の代替手段としては、文字列のマッチングや置換に使用できるQuake Regular Expressionや、Perl Compatible Regular Expression (PCRE)などがあります。Fish Shellでは、主にPOSIXの基本正規表現をサポートしています。

## 関連リンク：
- 魚シェルガイド正規表現のセクション：https://fishshell.com/docs/current/index.html#regexp
- 魚シェルガイド文字列置換のセクション：https://fishshell.com/docs/current/index.html#string-replace
- 魚シェルガイド文章の一致のセクション：https://fishshell.com/docs/current/index.html#string-match