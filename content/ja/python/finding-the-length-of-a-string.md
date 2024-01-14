---
title:                "Python: 「文字列の長さを求める方法」"
simple_title:         "「文字列の長さを求める方法」"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列の長さを見つけることに興味があるのか、その理由を簡単に説明します。

## 方法
「```Python ... ```」のコードブロック内に、コーディングの例やサンプルの出力を記載しました。

例えば、`len()`関数を使用して文字列の長さを取得することができます。以下のようにコードを記述します。

```Python
string = "こんにちは、世界！"
print(len(string))
```

出力結果は、`12`になります。つまり、「こんにちは、世界！」という文字列は12文字で構成されていることがわかります。

## ディープダイブ
文字列の長さを取得する方法について、もう少し詳しく見てみましょう。

`len()`関数は、文字列の長さを返すだけでなく、リストやタプルなど他のデータ型の長さも取得することができます。また、日本語の文字列の場合、文字コードによっては文字数が2文字分とカウントされることに注意が必要です。

## 参考リンク
- [Python公式ドキュメント: len()関数](https://docs.python.org/ja/3/library/functions.html#len)
- [Programiz: Pythonで文字列の長さを取得する](https://www.programiz.com/python-programming/methods/string/len)