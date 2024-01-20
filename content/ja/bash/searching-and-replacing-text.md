---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何となぜ?

テキストの検索と置換は、特定のパターンや語句を見つけ出し、必要に応じて新しいテキストで置き換えることです。これは、不要な部分を取り除いたり、バグを修正したり、コードの単純化や標準化を行うためにプログラマーが利用します。

## 使い方:

検索と置換を行うための基本的なBashコマンドは`sed`です。以下に一部コード例とその出力を示します。

```Bash
# 'Hello, World!' から 'World' を 'Japan' に置換してみましょう
echo 'Hello, World!' | sed 's/World/Japan/'
```

これは、`Hello, Japan!`という出力結果を示します。

## ディープダイブ:

`sed`は、1970年代から存在し、UNIXの一部として開発されました。その歴史の長さにより、様々なパターン置換の能力を持つツールとして成熟しています。

しかし、替わりに`awk`, `grep`, `perl`のような別のツールを利用することも可能です。それらは、より複雑なパターンマッチングやテキスト処理の能力を持っています。

`sed`の基本的な書式は `s/検索パターン/置換パターン/` です。ここで's'はsubstitute（置換）を表します。また、このコマンドにはさまざまなフラグが付けられ、例えば 'g'フラグは、行内のすべてのマッチを置換します。

## 参考リンク:

- sedの詳細: https://www.gnu.org/software/sed/manual/sed.html
- 正規表現について: https://developer.mozilla.org/ja/docs/Web/JavaScript/Guide/Regular_Expressions
- 置換コマンドについてのより詳しい情報: https://www.cyberciti.biz/faq/how-to-use-sed-to-find-and-replace-text-in-files-in-linux-unix-shell/