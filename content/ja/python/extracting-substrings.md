---
title:                "Python: 「部分文字列の抽出」"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜサブストリングを抽出するのか

サブストリングを抽出するとは、文字列の一部分を取り出すことを指します。これは、文字列を操作する際に非常に便利です。例えば、特定のワードを検索するために文字列内の特定の部分を抽出するなど、様々な場面で活用することができます。

## サブストリングを抽出する方法

Pythonでは、文字列をスライスすることでサブストリングを抽出することができます。スライスは、リストや文字列などのシーケンスオブジェクトから任意の要素を取り出すための方法です。下記のコードを参考に、サブストリングを抽出する方法をご紹介します。

```Python
# サブストリングを抽出する例
my_string = "こんにちは、私はPythonを勉強しています"

# "Python"を抽出する
print(my_string[8:14])
# 出力結果: "Python"
```

上記の例では、文字列内の8文字目から14文字目までを取り出しています。スライスの範囲指定では、最初の数値は含み、最後の数値は含まれないことに注意してください。

## サブストリングの詳細

さらに、サブストリングを抽出する際にはさまざまな方法があります。例えば、整数や文字列を用いて指定位置から指定文字数分を抽出する方法や、特定の文字列を検索してその周辺の文字列を抽出する方法などがあります。これらの方法を詳しく学ぶことで、より精密な文字列操作が可能になります。

## see also

- Python公式ドキュメント: https://docs.python.org/ja/3/library/stdtypes.html#typesseq
- プログラミング初心者向けのサンプルコード集: https://qiita.com/kyuna/items/73106ab53c83cc373fb8#%E6%8A%BD%E5%87%BA
- スライスについての解説記事: https://note.nkmk.me/python-slice-usage/