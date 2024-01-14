---
title:                "C#: 連結する文字列"
simple_title:         "連結する文字列"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ文字列を連結するのか

文字列の連結は、よくあるタスクの一つです。例えば、ユーザーからの入力をデータベースに保存するときや、文書を作成するときに使用します。文字列を連結することで、情報を効率的に扱うことができます。

## 方法

文字列を連結するためには、C#のConcatメソッドを使用します。例えば、次のようなコードを記述することで、2つの文字列を連結することができます。

```C#
string firstName = "太郎";
string lastName = "山田";
string fullName = string.Concat(firstName, lastName);
Console.WriteLine(fullName);
```

出力結果は「太郎山田」となります。

## 深い掘り下げ

文字列を連結する際には、メモリの使用に関して注意する必要があります。例えば、大量の文字列を連結する場合、StringクラスのConcatメソッドではなく、StringBuilderクラスのAppendメソッドを使用することで、メモリ使用量を最適化することができます。

また、文字列の連結には、ストリングフォーマットや文字列補完といった便利な方法もあります。これらを使用することで、より簡潔なコードを記述することができます。

## 併せて読みたい

- [C#のStringクラスのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.concat)
- [C#のStringBuilderクラスのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.text.stringbuilder)
- [C#の文字列操作に関する記事](https://qiita.com/minato-naka/items/72a3579dcbafadd2341b)