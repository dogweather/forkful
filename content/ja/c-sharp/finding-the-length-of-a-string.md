---
title:                "C#: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# なぜ文字列の長さを調べる必要があるのか

文字列の長さを調べることは、プログラミングにおいて非常に一般的な作業です。例えば、文字列を入力するフォームで制限された文字数内に入力されているかをチェックする、テキストデータを正しく処理するために必要な作業などが挙げられます。文字列の長さを知ることで、プログラムをより正確に制御することができます。

## 方法

文字列の長さを調べるには、C#の `Length` プロパティを使用します。例えば、以下のようなコードを記述することで、文字列の長さをコンソールに出力することができます。

```C#
string str = "こんにちは";
Console.WriteLine(str.Length);
```

この場合、`str` の長さである「5」が出力されます。

また、文字列の長さを調べる際には空白も含まれることに注意しましょう。例えば、`Hello World` という文字列の長さは11となります。

## 深堀り

文字列の長さを調べる際には、文字コードの種類にも注意が必要です。たとえば、英語の場合は1文字あたり1バイトとなりますが、日本語の場合は2バイトとなります。そのため、日本語の文字列の長さを調べる際には、`Encoding.GetByteCount()` メソッドを使用することで正確な長さを取得することができます。

# 参考リンク

- [C# Length プロパティのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.length)
- [C# Encoding.GetByteCount() メソッドのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.text.encoding.getbytecount)
- [文字コードの種類による文字列の長さ取得の違い](https://www.atmarkit.co.jp/ait/articles/1609/28/news031.html)

## 参考

[C#プログラミング入門 - 文字列を扱う](https://www.javadrive.jp/csharp/string/index1.html)