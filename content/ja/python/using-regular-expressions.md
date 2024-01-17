---
title:                "正規表現を使用する"
html_title:           "Python: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何が何故?

プログラマーにとって、正規表現は非常に便利なツールです。正規表現とは、特定のパターンに一致する文字列を検索・抽出するための方法です。プログラマーが正規表現を使う理由は、大量のテキストデータから必要な情報を効率的に取得するためです。

## 使い方:

```Python
# 文字列の中から特定のパターンを検索する例
import re

text = "ユーザー名: John123, パスワード: abc123"
pattern = r"ユーザー名: (\w+), パスワード: (\w+)"
# \wは任意の単語文字を表す特殊文字です。
# ()はグループ化して後で取り出すためのものです。

match = re.search(pattern, text)
# search関数は、文字列の中からパターンに一致する最初の部分を探し出します。
# matchオブジェクトが返されます。

username = match.group(1)
# group(1)はパターンの最初のグループを取り出します。
password = match.group(2)

print(f"ユーザー名: {username}, パスワード: {password}")
# 出力: ユーザー名: John123, パスワード: abc123
```

## 深堀り:

正規表現は、1960年代に計算機科学者のKen Thompsonと他の研究者によって開発されました。現在では、ほとんどのプログラミング言語やテキストエディタなどで使用することができます。正規表現には他にも様々なパターンマッチングの手法がありますが、プログラマーにとって最も普及しているのは正規表現でしょう。

正規表現を使う際に注意すべき点として、特殊文字をエスケープする必要があることが挙げられます。例えば、ドット(.)やバックスラッシュ(\)などは特殊文字として解釈されるため、文字として検索したい場合はバックスラッシュを前に付ける必要があります。

## 関連リンク:

- [正規表現チートシート (Python)](https://www.debuggex.com/cheatsheet/regex/python)
- [RegexOne - Learn Regular Expressions with simple, interactive exercises](https://regexone.com/)
- [Python reモジュール公式ドキュメント](https://docs.python.org/ja/3/library/re.html)