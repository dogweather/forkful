---
title:                "文字列の大文字化"
html_title:           "C#: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列の大文字化とは文字列内の各単語の最初の文字を大文字にすることです。これはデータの一貫性を保つため、またはプログラムの出力を整形するためにプログラマーが行います。

## 方法：
C#では`ToUpper()`または`TextInfo.ToTitleCase()`メソッドで文字列を簡単に大文字化できます。 

```C#
string name = "hello world";
// Using ToUpper method
string upperName = name.ToUpper();
Console.WriteLine(upperName); // Output: HELLO WORLD

// Using TextInfo.ToTitleCase method
System.Globalization.CultureInfo cultureInfo = System.Threading.Thread.CurrentThread.CurrentCulture;
System.Globalization.TextInfo textInfo = cultureInfo.TextInfo;
string properName = textInfo.ToTitleCase(name);
Console.WriteLine(properName); // Output: Hello World
```

## 深堀り
- **歴史**: 文字列の大文字化はプログラミングの初期から存在しています。
- **代替手段**: 大文字化には他の方法もありますが、C#ではこれらのメソッドが最も直接的で手軽に利用できます。
- **実装詳細**: `ToUpper()`メソッドは文字列すべてを大文字にします。一方、`TextInfo.ToTitleCase()`は文字列内の各単語の最初の文字を大文字にします。

## 参考情報：
- [Microsoft Docs: TextInfo.ToTitleCase](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase)
- [Microsoft Docs: String.ToUpper](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)