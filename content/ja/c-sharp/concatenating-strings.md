---
title:    "C#: 文字列の結合"
keywords: ["C#"]
---

{{< edit_this_page >}}

**なぜ**: プログラマーにとって、文字列を結合することは非常に重要なタスクです。文字列を単純に結合するだけでも、プログラムの複雑性を低く保ち、コードをより簡潔に保つことができます。

## 使い方

文字列を結合する最も一般的な方法は、C#の組み込みメソッドである `string.Concat()` を使用することです。このメソッドは、2つ以上の文字列を引数として受け取り、それらを順番に結合して新しい文字列を作成します。

```C#
// 文字列を結合する
string str1 = "今日は";
string str2 = "いい天気です";
string result = string.Concat(str1, str2);

// 結果: 今日はいい天気です
Console.WriteLine(result);
```

または、文字列を `+` オペレーターを使用して結合することもできます。この方法は比較的簡単であり、C#の他のデータ型と同じように使用することができます。

```C#
// + オペレーターを使用して文字列を結合する
string str1 = "こんにちは";
string str2 = "、私の名前は";
string str3 = "太郎です";
string result = str1 + str2 + str3;

// 結果: こんにちは、私の名前は太郎です
Console.WriteLine(result);
```

さらに、 `string.Format()` メソッドを使用することで、より柔軟な文字列の結合が可能です。このメソッドは、指定したフォーマットに従って文字列を結合することができます。

```C#
// string.Format() を使用して文字列を結合する
string str1 = "私の名前は{0}です。来週、{1}歳になります。";
string result = string.Format(str1, "太郎", 26);

// 結果: 私の名前は太郎です。来週、26歳になります。
Console.WriteLine(result);
```

## 深堀り

文字列を結合する際、メモリの効率性を考慮することも重要です。実際、 `string.Concat()` メソッドは、文字列を新しいメモリ領域にコピーします。これは、文字列が大きくなるほどメモリ使用量が増加することを意味します。

そこで、 `StringBuilder` クラスを使用することで、メモリの効率性を改善することができます。これは可変長の文字列を操作するクラスであり、新しい文字列を作成せずに既存の文字列を編集することができます。

```C#
// StringBuilder を使用して文字列を結合する
StringBuilder sb = new StringBuilder();
sb.Append("私の名前は");
sb.Append("太郎");
sb.Append("です");

// 結果: 私の名前は太郎です
Console.WriteLine(sb.ToString());
```

また、文字列結合においては文字列のパフォーマンスも重要です。例えば、ループ内で文字列を `+` オペレーターで結合することは、パフォーマンスに悪影響を与える可能性があります。そのため、できるだけ `StringBuilder` または `string.Format()` を使用するよう心がけましょう。

## 参考リンク

- [C# String Concatenation: Why and How to](https://www.c-sharpcorner