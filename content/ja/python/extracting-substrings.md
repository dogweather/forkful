---
title:                "サブストリングの抽出"
html_title:           "Python: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何をするのか？なぜそうするのか？

抽出した文字列の取得とは、文字列の一部分を切り出すことを指します。プログラマーがこの作業を行うのは、与えられた文字列から特定の情報を取り出す場合や、文字列を処理するために必要な部分を取り出す場合などがあります。

## 方法：

実際にコードを書く前に、Pythonで文字列を抽出する方法を説明します。まず、抽出したい文字列を指定します。例えば、次のように記述します。

```Python
string = "Pythonプログラミングが好きです"
```

次に、抽出したい文字列が何文字目から何文字目までなのかを指定します。例えば、次のように記述します。

```Python
substring = string[6:14]
```

最後に、取得したい部分の長さを指定する方法もあります。

```Python
substring = string[6:len(string)]
```

抽出した文字列を取得することができます。

## 深堀り：

抽出した文字列を取得する方法は、Pythonのビルトインの機能の一つであり、よく使われています。しかし、他の方法もあります。例えば、正規表現を使用することで、より強力な文字列の抽出が可能です。

また、Pythonの文字列はイミュータブルであり、各文字は個別にアクセスできるので、抽出方法が柔軟になります。

さらに、抽出した文字列を変更することもできます。例えば、次のように記述します。

```Python
# 文字列の一部を大文字に変更
string = string[6:14].upper()
print(string) # 出力: プログラミング
```

## 関連リンク：

- [Pythonの文字列操作](https://docs.python.org/ja/3/library/stdtypes.html#text-sequence-type-str)
- [正規表現のチュートリアル](https://docs.python.org/ja/3/howto/regex.html)
- [Pythonの文字列のイミュータブル性について](https://www.geeksforgeeks.org/g-fact-98/)