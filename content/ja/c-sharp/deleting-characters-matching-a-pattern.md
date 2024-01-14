---
title:                "C#: パターンに一致する文字の削除"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

文字列のパターンにマッチする文字を削除することは、プログラミングにおいて非常に重要です。例えば、コード内の余分なスペースや特定の文字列を自動的に削除することで、コードの可読性を向上させることができます。また、特定の条件に基づいて文字を削除することで、データの整形や検索を効率的に行うことができます。

## 方法

まずは、プログラム内で使用する文字列を宣言します。

```C#
string text = "アイウエオ かきくけこ 美味しい チョコレート";
```

次に、削除したい文字のパターンを指定します。例えば、半角スペースやひらがなの文字を削除したい場合は、正規表現を使用することができます。

```C#
Regex regex = new Regex(@"[\sぁ-ん]");
```

最後に、`Regex.Replace()`メソッドを使用して削除処理を行います。

```C#
string result = regex.Replace(text, "");
Console.WriteLine(result);

// 出力結果：美味しいチョコレート
```

## ディープダイブ

今回の例では、`Regex.Replace()`メソッドを使用しているため、パターンにマッチした文字を削除することができます。しかし、このメソッドにはさらに多くのオプションがあり、より詳細な削除処理を行うことが可能です。

例えば、第三引数に置換用の文字列を指定することで、マッチした文字を任意の文字列に置き換えることができます。また、第四引数に`RegexOptions.IgnoreCase`を指定することで、大文字と小文字を区別せずにパターンをマッチさせることもできます。

詳細な情報や例は、MSDNの正規表現クラスのドキュメントを参照してください。

## 他の参考リンク

- [MSDN 正規表現クラスのドキュメント](https://msdn.microsoft.com/ja-jp/library/system.text.regularexpressions.regex(v=vs.110).aspx)
- [C#で正規表現を使用する方法 - Qiita](https://qiita.com/yuka58sat/items/a93c5e249910b5a2f31c)