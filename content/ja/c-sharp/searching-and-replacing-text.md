---
title:    "C#: 「テキストの検索と置換」"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストを検索して置換するということには、コンピュータープログラミングにおいて非常に便利な理由があります。テキストを一度に多くの場所で同時に変更することができますし、手作業で行うよりもはるかに高速です。

## ハウツー

テキストの検索と置換は、C#言語において非常に簡単に実現することができます。まずは、検索条件となる文字列を指定し、置換したい文字列を指定します。以下のコード例を参考にしてください。

```C#
string inputText = "こんにちは、今日はいい天気ですね。";
string searchText = "いい";
string replaceText = "すばらしい";

string outputText = inputText.Replace(searchText, replaceText);

Console.WriteLine(outputText);
```

出力結果は以下のようになります。

```
こんにちは、今日はすばらしい天気ですね。
```

## ディープダイブ

テキストの検索と置換では、他にも様々なオプションが用意されています。例えば、大文字と小文字を区別するかどうかや、検索結果を制限するための正規表現を使用することもできます。さらに、一度に複数の文字列を置換することも可能です。必要に応じて、より詳しい情報を調べることができます。

## 参考リンク

- [C#における文字列の検索と置換の方法 (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/how-to-search-strings)
- [C#での正規表現の使用 (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expression-usage)
- [C#の文字列操作に関するベストプラクティス (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/best-practices-strings)


## 関連リンク

- [C#でファイルの読み書きをする方法](https://www.onlineclassnotes.com/2018/02/csharp-how-to-read-and-write-files-using-streamreader-and-streamwriter.html)
- [C#で文字列を比較する方法](https://www.guru99.com/c-sharp-string-compare.html)
- [C#のデバッグ方法 (Microsoft Docs)](https://docs.microsoft.com/ja-jp/visualstudio/debugger/debugger-feature-tour?view=vs-2019)