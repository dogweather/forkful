---
title:                "部分文字列の抽出"
html_title:           "C#: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何ができて、なぜやるのか？

サブ文字列を抽出するとは、文字列から特定の部分文字列を取り出すことを指します。プログラマーがこれを行うのは、より柔軟な文字列操作を実現するためです。

## 方法：

```C#
// 文字列の一部を抽出する文字数を指定して抽出する方法
string str = "こんにちは！私はプログラマーです。";
string extractedStr = str.Substring(3, 5);
// 出力：はじめま

// 文字列内の文字で部分文字列を抽出する方法
// (例：「はじめま」の「は」を抽出)
string extractedChar = extractedStr.Substring(0, 1);
// 出力：は
```

## 深く掘り下げる

抽出したい部分文字列の位置を指定することで、Substringメソッドを使用して任意の長さのサブ文字列を取得することができます。この機能は、文字列操作やデータ処理の際に非常に役立ちます。他のアルゴリズムやデータ構造を使用する代わりに、サブ文字列を抽出することでより柔軟なコーディングを実現できます。

## 関連リンク

- [C# ドキュメント - Substring メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.substring)
- [C# サブ文字列の例](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/how-to-extract-a-substring-from-a-string)
- [C# の文字列操作のチュートリアル](https://www.tutorialsteacher.com/csharp/csharp-string)