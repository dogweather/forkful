---
title:                "C#: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ
文字列からパターンにマッチする文字を削除することのメリットについてご紹介します。

## 使い方
文字列から特定のパターンにマッチする文字を削除する方法はいくつかありますが、ここではC#を使用したコーディング例をご紹介します。まずは```C#
string text = "Hello World! こんにちは！";
string pattern = "[A-Za-z]+";
string result = Regex.Replace(text, pattern, "");
Console.WriteLine(result); // こんにちは！
``` 
というコードを書きます。このコードでは、文字列から英字を全て削除することができます。もちろん、パターンを変えることで削除する文字を自由に設定することも可能です。

## 詳しく見る
文字列からマッチする文字を削除する方法について深く知りたい方は、正規表現(Regex)について学ぶことをお勧めします。正規表現を使用することで、より高度なパターンマッチングが可能になります。また、文字列操作に関するC#の基本的な知識も必要になりますので、必要に応じて学習されることをお勧めします。

## 参考リンク
- [正規表現チュートリアル (C#)](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [C#入門講座 ～文字列編～](https://www.javadrive.jp/csharp/string/index4.html)
- [C#での文字列処理の基礎知識](https://ufcpp.net/study/csharp/string/basic/)