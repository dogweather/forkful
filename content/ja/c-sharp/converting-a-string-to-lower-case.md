---
title:    "C#: 文字列を小文字に変換する"
keywords: ["C#"]
---

{{< edit_this_page >}}

こんにちは、C#プログラマーの皆さん！今日はみんなで一緒に、文字列を小文字に変換する方法についてお話ししましょう。

## なぜ文字列を小文字に変換するのか
文字列を小文字に変換することで、文字列の比較や処理がより簡単になります。また、データベースなどの外部ソースから受け取った文字列が大文字や混在している場合、統一された小文字に変換することでプログラミング上のエラーを防ぐことができます。

## 方法：コード例と出力結果
文字列を小文字に変換するためには、C#プログラムの`ToLower()`メソッドを使用します。下記の例をご覧ください。

```C#
string text = "HELLO, WORLD!";
string result = text.ToLower();

Console.WriteLine(result);
// 出力結果：hello, world!
```

上記のように、`ToLower()`メソッドを使用することで、元の文字列を小文字に変換した新しい文字列が作成されます。

もしもあなたが文字列の大文字や小文字を気にせずに比較したい場合は、`Equals()`メソッドを使用することで、大文字と小文字を区別しない比較が可能です。例を見てみましょう。

```C#
string text1 = "hello";
string text2 = "HELLO";

Console.WriteLine(text1.Equals(text2));
// 出力結果：False
Console.WriteLine(text1.ToLower().Equals(text2.ToLower()));
// 出力結果：True
```

## 詳しく見てみよう
`ToLower()`メソッドは、文字列を小文字に変換するだけでなく、例えば`ToString()`メソッドで数値型のデータを文字列に変換した場合でも、その文字列を小文字に変換してくれます。

また、多言語プログラムでは、英語以外の文字が含まれる可能性があるため、`ToLower()`メソッドでは英語以外の言語でも正しい小文字に変換してくれるようになっています。

## それでは、実際に使ってみよう
今回お話しした`ToLower()`メソッドは、実践的なプログラミングでよく使用されます。ぜひ日常的なプログラミングで試してみてください。

この記事では、文字列を小文字に変換する方法とその利点についてお話ししました。C#プログラミングの基礎として、ぜひ覚えておいてくださいね。

## 参考リンク
- [C#公式ドキュメント（英語）](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- [C#ドキュメンテーション（日本語）](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.tolower?view=net-5.0)

以上で今日の記事は終了です。ご覧いただきありがとうございました。

## 関連記事
- [C#の文字列操作方法について学ぼう（日本語）](https://www.javadrive.jp/csharp/string/index2.html)
- [C#で文字列を比較する方法（日本語）](https://code-maze.com/c-sharp-string-comparison/)