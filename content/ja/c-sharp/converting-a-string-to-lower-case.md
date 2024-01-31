---
title:                "文字列を小文字に変換"
date:                  2024-01-20T17:37:53.944411-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列を小文字に変換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ？

文字列を小文字に変換することは、プログラミングにおいて、大文字と小文字を区別しない比較をしたい時や、データの一貫性を保つために行います。ユーザーが入力したテキストを整形する際にも使われます。

## How to:
## 方法:

```C#
string originalText = "Konnichiwa, Sekai!";
string lowerCaseText = originalText.ToLower();

Console.WriteLine(lowerCaseText);
// 出力: konnichiwa, sekai!
```

## Deep Dive
## 詳細情報:

小文字変換は長い歴史を持ち、プログラミング言語が発展するにつれて進化してきました。C#では`ToLower()`や`ToLowerInvariant()`といったメソッドを使って実現します。`ToLowerInvariant()`はカルチャーに依存しないケース変換を提供し、異なるカルチャー間でも一貫性を保つために使用されます。中には`TextInfo`クラスを使ったより細かい制御を行う方法もありますが、ほとんどの場面では`ToLower()`で事足ります。

## See Also
## 関連リンク:

- [Microsoft's official ToLower documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-6.0)
- [Microsoft's guide to globalization and localization](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/)
- [Stack Overflow: Why use ToLowerInvariant over ToLower?](https://stackoverflow.com/questions/6225808/string-tolower-and-string-tolowerinvariant)
