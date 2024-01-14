---
title:    "C#: 正規表現の使用"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使うメリットについて説明します。正規表現を使用することで、文字列のパターンを簡単にマッチングしたり、検索したりすることができます。また、コードをより簡潔に記述できるので、作業効率も向上します。

## 使い方

"```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string text = "今日はいい天気です。";
        string pattern = "今日";

        // 正規表現を使ってパターンのマッチングを行う
        Match match = Regex.Match(text, pattern);

        // マッチング結果を出力する
        Console.WriteLine("マッチングした文字列は " + match.Value + " です。");
    }
}
```
出力: マッチングした文字列は 今日 です。

## ディープダイブ

正規表現のパターンの書き方や特殊文字の意味など、詳細について説明します。また、正規表現のグループ化やマッチング条件の指定など、より高度な使い方も紹介します。正規表現をマスターすることで、より効率的に検索や置換ができるようになります。

## さらに見る

- [C#での正規表現の使い方 | Microsoft Docs](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [正規表現チュートリアル | W3Schools](https://www.w3schools.com/js/js_regex.asp)
- [C#正規表現の基礎知識 | codezine](https://codezine.jp/article/detail/1941)