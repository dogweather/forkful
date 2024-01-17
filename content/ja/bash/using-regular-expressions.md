---
title:                "正規表現の使用"
html_title:           "Bash: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なに&どうして？
正規表現を使うとは、あなたが文字列を検索や置換をするために使用する特殊な文字の集合です。プログラマーはこのツールを使うことで、文字列を効率的に操作することができます。

## 使い方：
正規表現を使うには、次のように記述します。

```Bash
# 文字列を検索する
grep 'パターン' ファイル名

# 文字列を置換する
sed 's/置換前/置換後/g' ファイル名
```

例えば、`grep 'Hello' hello.txt`と記述すると、`hello.txt`ファイルの中で"Hello"という文字列を検索することができます。また、`sed 's/Hello/こんにちは/g' hello.txt`と記述すると、同じファイルの中で"Hello"を"こんにちは"に置換できます。

## もっと深く：
正規表現はUnixの世界で生まれましたが、今ではさまざまなプログラミング言語やツールで使用することができます。代替手段として、ワイルドカードや文字列処理関数を使用することもできますが、正規表現はより柔軟かつ強力なツールです。実装の詳細については、各プログラミング言語のドキュメントを参照してください。

## 参考：
- [正規表現のチートシート](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Unixの正規表現の歴史](https://www.cs.princeton.edu/courses/archive/spring09/cos333/beautiful.html)