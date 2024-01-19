---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の長さを調べるとは、文字列中の文字数を特定することを指します。プログラマーがこの操作を行う理由は、文字列処理や検証などの多くのタスクにおいて、文字列のサイズが絶対的な知識が必要なからです。

## 方法：

以下に文字列の長さを調べるコードの例を示します：

```C#
string myString = "こんな感じ";
Console.WriteLine("文字列の長さ: " + myString.Length);
```

このプログラムを実行すると、「文字列の長さ: 5」という出力が得られます。

## ディープダイブ：

文字列の長さを計算する操作は、C#が初めて発表された時から存在しています。`Length`プロパティは文字列の長さを瞬時に返すため、非常に効率的です。これに対してループを用いて文字列の長さを手動で計算するなど、他の方法も存在しますが、非常に時間が掛かるうえにエラープローンです。

ただし、特別な状況（例えば特定の文字の数を数える必要がある場合等）では、自動の方法では対応できないため、そのような場合は手動で計算する方法をとる必要があります。

## 関連情報：

以下に、このトピックと関連するさらなる情報ソースのリンクを紹介します：

- C#公式ドキュメンテーション：[String.Length プロパティ](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.length?view=net-5.0)
- マイクロソフトのチュートリアル：[C#における文字列操作](https://docs.microsoft.com/ja-jp/dotnet/csharp/how-to/modify-string-contents)
- スタック・オーバーフローが提供するQA：[C#での文字列長の計算方法](https://stackoverflow.com/questions/906273/what-is-the-length-of-a-string-in-c-sharp)