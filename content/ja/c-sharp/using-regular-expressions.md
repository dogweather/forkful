---
title:                "C#: 正規表現の使用"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使う理由はさまざまですが、最も一般的な理由は、テキストファイルや文字列内の特定のパターンを検索したり抽出したりするために使用することです。例えば、メールアドレスや電話番号などの特定の形式のデータを抽出する際に、正規表現を使用することができます。

## 使い方

正規表現を使用するには、System.Text.RegularExpressions命名空間を使用します。また、Regexクラスを使ってパターンを定義し、それを使用して文字列を検索や抽出します。以下は、C#で正規表現を使った例です。

```C#
using System;
using System.Text.RegularExpressions;

public class Program
{
	public static void Main()
	{
		// 例：メールアドレスを抽出する
		string text = "私のメールアドレスはexample@gmail.comです。";
		string pattern = @"[\w-]+@([\w-]+\.)+[\w-]+";
		Match match = Regex.Match(text, pattern);
		Console.WriteLine(match.Value); // example@gmail,com
	}
}
```

上記の例では、まず検索対象のテキストとパターンを定義して、Regex.Matchメソッドで検索を行い、その結果を取得しています。

## ディープダイブ

正規表現を使う上でのヒントやテクニックをいくつか紹介します。

- ワイルドカードとしてのドット（.）：正規表現ではドット（.）を使うことで、任意の1文字にマッチさせることができます。例えば、ワイルドカードを使って"a.b"というパターンを定義すると、"axb"や"ayb"といった文字列にマッチします。
- パターン修飾子：マッチする文字列の制限を行うために、パターン修飾子を使うことができます。例えば、iをつけると大文字と小文字を区別せずに検索を行うことができます。
- 特殊文字のエスケープ：正規表現では、一部の文字が特殊文字として扱われます。そのため、その文字をそのまま検索する場合はバックスラッシュ（\）を使ってエスケープする必要があります。

これらの他にもたくさんの使い方やコツが存在しますので、積極的に使ってみることでよりスマートなコーディングを行うことができるでしょう。

## より詳しい情報

- 正規表現入門：https://www.w3schools.com/python/python_regex.asp
- Regexクラスのドキュメント：https://docs.microsoft.com/ja-jp/dotnet/api/system.text.regularexpressions.regex?view=netframework-4.8 
- 正規表現のチートシート：https://www.rexegg.com/regex-quickstart.html
- 正規表現の視覚化ツール：https://regexper.com/

## 参考リンク

- [入門：C#で正規表現を使う方法](https://qiita.com/fumi-ito/items/2f9dd02ebb50638fe2d7)
- [正規表現を使ったテキストの検索・置換](https://www.buildinsider.net/language/csharpregular/01)