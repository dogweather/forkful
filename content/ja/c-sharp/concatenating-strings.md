---
title:                "C#: 文字列の連結"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ 
文字列を連結することに取り組むか

文字列の連結は、プログラミングで非常に頻繁に使用される基本的なタスクです。複数の変数や情報を組み合わせて、新しい文字列を作成する必要がある場合に活用できます。例えば、ファイル名を作成する際や、テキストメッセージを組み立てる際に役立ちます。

## 使い方 
```C#
// プログラムで文字列を連結する方法
string firstName = "太郎";
string lastName = "山田";
string fullName = firstName + lastName;
Console.WriteLine(fullName);
```
出力結果: 太郎山田

文字列を連結する方法は簡単です。単純に、プラス記号（＋）を使用して文字列を結合することができます。また、変数を使用して動的な文字列を作成することもできます。

```C#
// 動的な文字列を作成する方法
int age = 25;
string message = "私の名前は" + firstName + lastName + "です。年齢は" + age + "歳です。";
Console.WriteLine(message);
```
出力結果: 私の名前は太郎山田です。年齢は25歳です。

## 詳細を調べる 
文字列を連結する方法は、プログラミング言語によって異なる場合があります。C#では、StringBuilderクラスを使用すると、パフォーマンスをより高めることができます。これは、文字列を連結するために新しいメモリ空間を確保する必要がないからです。また、文字列の連結には、メソッドチェーンを使用する方法もあります。

```C#
// StringBuilderを使用した場合
StringBuilder sb = new StringBuilder();
sb.Append("Hello");
sb.Append(" World");
sb.Append("!");
string result = sb.ToString();
Console.WriteLine(result);
```
出力結果: Hello World!

このように、StringBuilderクラスを使用することで、複数の文字列を連結する場合に便利です。ただし、コードが複雑になる場合はメソッドチェーンを使用した方が良いでしょう。

## 他の記事を参照 
この記事では、C#で文字列を連結する方法について説明しましたが、他のプログラミング言語でも同様の方法で行うことができます。次のリンクを参考にしてください。

- [Javaで文字列を連結する方法](https://qiita.com/masato44gm/items/771bcca2ff413cde2ade)
- [Pythonで文字列を連結する方法](https://note.nkmk.me/python-string-concat/)
- [JavaScriptで文字列を連結する方法](https://qiita.com/tattn/items/b8bf497d730fd49305f4)

## 参照 
- [C#で文字列を扱う](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/strings/)