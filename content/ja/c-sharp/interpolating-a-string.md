---
title:                "文字列を補間する"
html_title:           "C#: 文字列を補間する"
simple_title:         "文字列を補間する"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の内挿（インターポレーション）とは何か、そしてプログラマーがそれを行う理由を２〜３文で説明します。

文字列の内挿は、変数や式を文字列の中に直接埋め込むことで、より簡潔かつ効率的に文字列を組み立てることができます。プログラマーがこの手法を使用する主な理由は、可読性の向上とコードのシンプル化です。

## 方法：

下記のような```C# ... ```のコードブロックを使用して、文字列の内挿を行う方法を示します。

```
string name = "太郎";
int age = 20;
string message = $"こんにちは、私の名前は{name}です。{age}歳です。";
Console.WriteLine(message);
```

出力結果は以下のようになります。

```
こんにちは、私の名前は太郎です。20歳です。
```

## 深堀り：

文字列の内挿は、C# 6.0で新たに導入された機能です。以前は、文字列の中に変数や式を埋め込むには、コンカチェナション（文字列の結合）を使用する必要がありました。しかし、コンカチェナションは複数の変数を組み合わせる時に不便であったため、文字列の内挿が導入されました。

文字列の内挿以外にも、プレースホルダーを使用したフォーマット文字列や、String.Formatメソッドを使用したフォーマット文字列の生成などの代替手段があります。

文字列の内挿の実装方法については、C#のコンパイラや実行時のデバイスによって若干の違いがありますが、基本的には文字列の中に変数や式を挿入することで行われます。

## 関連情報：

- [Expression-Bodied Methods and Properties in C# 6](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/statements-expressions-operators/expression-bodied-members#interpolated-strings)
- [String interpolation in C# 6.0](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/string-interpolation)
- [How to use string interpolation in C#](https://www.c-sharpcorner.com/article/string-interpolation-explained-with-examples-in-c-sharp/)