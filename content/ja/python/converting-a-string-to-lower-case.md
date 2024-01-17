---
title:                "文字列を小文字に変換する"
html_title:           "Python: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
文字列を小文字に変換することは、プログラマーがよく行う作業の一つです。この作業は、大文字と小文字が区別される場合に、文字列を統一的に処理するために行われます。

## 方法：
文字列を小文字に変換するには、Pythonの組み込み関数である```lower()```を使用します。以下の例を参考にしてください。

```Python
hello = "Hello World"
print(hello.lower()) # 出力: hello world
```

## 詳細を掘り下げる：
文字列を小文字に変換する方法は、プログラミング言語によって異なります。Pythonでは、文字列型のメソッドである```lower()```を使用することで簡単に実現することができます。これは、PythonがUnicodeをサポートしているためです。

他のプログラミング言語では、文字列を小文字に変換するために別の関数やメソッドを使用する必要があるかもしれません。例えば、C言語では```tolower()```関数を使用します。

## 関連情報：
- [Pythonの公式ドキュメント](https://docs.python.org/ja/3/library/stdtypes.html#str.lower)
- [Unicodeに関する情報](https://unicode.org/charts/)

---

本記事では、Pythonで文字列を小文字に変換する方法について紹介しました。この作業は、プログラマーにとって非常に便利であり、コードをより一貫性のあるものにすることができます。Pythonの```lower()```関数を活用して、より効率的なプログラミングを楽しんでください！