---
title:                "Python: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ正規表現を使用するのか

正規表現は、特定のパターンを持つ文字列を簡単に検索や操作することができる便利なPythonツールです。例えば、メールアドレスや電話番号などを簡単に抽出することができます。

## 正規表現の使用方法

正規表現を使用するには、Pythonのreモジュールをインポートする必要があります。次に、re.search()関数を使用して、検索したいパターンと文字列を指定します。

```Python
import re

text = "私の電話番号は123-456-7890です。"
pattern = r"\d{3}-\d{3}-\d{4}"

match = re.search(pattern, text)

print(match.group()) # 出力結果は"123-456-7890"
```

この例では、\dは数字を表し、{3}は3回繰り返すことを意味します。そのため、\d{3}は3桁の数字を表すことになります。また、search()関数では最初に見つかったパターンのみを返すため、メールアドレスなど複数のパターンを検索する場合はfindall()関数を使用します。

## 正規表現の詳細

正規表現には様々な特殊文字やメタ文字があり、それぞれの意味や使い方については深く学ぶことができます。また、正規表現を使用することで文字列の分割や置換など多くのことが行えるため、より高度なプログラミングテクニックとしても活用することができます。

## もっと詳しく学ぶ

- [Python reモジュールのドキュメント](https://docs.python.org/ja/3/library/re.html)
- [正規表現チュートリアル](https://docs.python.org/ja/3/howto/regex.html)
- [Pythonで正規表現を使用する方法](https://www.geeksforgeeks.org/how-to-use-regular-expression-in-python/)
- [正規表現の正しい使い方](https://www.programiz.com/python-programming/regex)