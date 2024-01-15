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

## なぜ

抽出されたサブストリングを行う理由は何でしょうか？Pythonプログラムを作成する場合、文字列を操作する必要があることがあります。文字列から特定の文字や単語を取り出したい場合、あるいは文字列を分割したい場合などに抽出されたサブストリングが役に立ちます。

## 方法

抽出されたサブストリングを行う方法について説明します。まずは文字列を定義します。次に、```indexOf```メソッドを使用して抽出したい文字列のインデックスを取得します。最後に、```substring```メソッドを使用して抽出されたサブストリングを取得します。

```Python
# 文字列の定義
string = "こんにちは、私はPythonプログラミングを学んでいます。"

# "Python" のインデックスを取得
index = string.indexOf("Python")

# "Python" から右側の文字列を抽出
substring = string.substring(index, len(string))

# 出力
print(substring) # "Pythonプログラミングを学んでいます。"
```

## ディープダイブ

抽出されたサブストリングを行う際に役立ついくつかのコマンドを紹介します。

- ```len```メソッドを使用して文字列の長さを取得することができます。
- ```replace```メソッドを使用して特定の文字を置き換えることができます。
- ```split```メソッドを使用して文字列を分割することができます。

これらのコマンドを組み合わせることでさまざまな抽出が可能になります。

## 他にも見るべき

- [Pythonの公式ドキュメント](https://docs.python.org/ja/3/library/stdtypes.html#textseq)
- [Pythonチュートリアル](https://docs.python.org/ja/3/tutorial/index.html)