---
title:    "C#: 文字列の抽出"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を抽出する理由はさまざまです。多くの場合、より大きな文字列から必要な情報を取り出したいときに使われます。例えば、電話番号やメールアドレスなど、特定の形式で表される情報を見つけるために、文字列を抽出することができます。

## 方法

C#では、文字列を抽出するためにSubstring()メソッドを使用することができます。まず、抽出したい部分の開始位置と終了位置を指定して、Substring()メソッドを呼び出します。例えば、以下のコードは、文字列の一部を抽出する方法を示しています。

```C#
// 元の文字列
string str = "こんにちは、私の名前は山田太郎です。";

// 先頭から7文字目以降を抽出
string substr1 = str.Substring(7);
Console.WriteLine(substr1);
// 出力: 私の名前は山田太郎です。

// 4文字目から9文字目を抽出
string substr2 = str.Substring(3, 5);
Console.WriteLine(substr2);
// 出力: 一の名前

```

## 深堀り

Substring()メソッドは、最初の引数に開始位置を指定し、第二引数に文字数を指定することで、任意の部分文字列を抽出することができます。また、EndsWith()メソッドやContains()メソッドと組み合わせることで、特定の文字列を含む部分文字列を抽出することもできます。

例えば、以下のコードは、電話番号を抽出する例です。

```C#
// 元の文字列
string str = "私の電話番号は080-1234-5678です。";

// 電話番号を抽出
int start = str.IndexOf("私の電話番号は") + 6;
int end = str.IndexOf("です。");
string number = str.Substring(start, end - start);
Console.WriteLine(number);
// 出力: 080-1234-5678
```

## その他

抽出した文字列を加工する場合は、StringBuilderクラスを使用すると、パフォーマンスを改善することができます。また、正規表現を使うことで、より柔軟に文字列を抽出することができます。

## 参考リンク

- [MSDN: Substring メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.substring)
- [TechAcademy Magazine: C# における正規表現入門](https://techacademy.jp/magazine/18491)
- [学習サイトドットネット: StringBuilderクラスの使い方](https://p-site.net/c-sharp/s-stringbuilder-class.html)