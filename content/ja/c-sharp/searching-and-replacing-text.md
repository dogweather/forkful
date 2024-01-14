---
title:                "C#: テキストの検索と置換"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# なぜ検索と置換テキストを行うのか

検索と置換テキストは、コード内の特定の文字列を簡単に見つけて変更することができる便利な機能です。これにより、コードをより効率的に編集することができるため、開発者にとって非常に重要なものです。

# 方法

```C#
// テキストを置換する
string text = "こんにちは、世界！";
text = text.Replace("こんにちは", "こんばんは");
Console.WriteLine(text)
// 出力： こんばんは、世界！

// 文字列内の特定の文字を検索する
string text = "Hello, world!";
int index = text.IndexOf("world");
Console.WriteLine($"\"world\"はtextの{index}番目にあります");
// 出力："world"はtextの7番目にあります
```

# ディープダイブ

検索と置換は、文字列内での操作だけでなく、ファイル内でも使用することができます。例えば、複数のファイル内で特定の文字列を置換する場合は、Visual Studioの検索と置換ウィンドウを使用することができます。また、正規表現を使用して複雑なパターンの置換も行うことができます。検索と置換は、効率的なコード編集のために必要不可欠なツールです。

# さらに参考になる情報

# 参考リンク
- https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0
- https://docs.microsoft.com/en-us/dotnet/api/system.string.indexof?view=net-5.0
- https://www.c-sharpcorner.com/article/regular-expressions-in-c-sharp/
- https://docs.microsoft.com/en-us/visualstudio/ide/working-with-code-files/managing-lists-of-search-results-in-the-find-and-replace-window?view=vs-2019