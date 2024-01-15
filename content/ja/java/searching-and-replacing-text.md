---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ?

テキストを検索して置換する必要があるかもしれません。例えば、大きな文書がある場合や複数のファイルに同じ修正が必要な場合など、手作業での修正は非常に時間がかかります。コンピューターを使って自動的に検索して置換することで、より効率的に作業を行うことができます。

## 使い方

まず、Javaの `String` クラスの `replace()` メソッドを使用して、テキスト内の特定の文字を置換する方法を見ていきましょう。次のコードを使用して、テキスト内の "Hello" を "こんにちは" に置換する例を示します。

```Java
String text = "Hello World!";
String replacedText = text.replace("Hello", "こんにちは");

System.out.println(replacedText); // Output: こんにちは World!
```

さらに、`replace()` メソッドは正規表現を使用して複数の文字を一度に置換することもできます。次のコードでは、"Hello" と "Hi" を一度に "こんにちは" に置換しています。

```Java
String text = "Hello World! Hi there!";
String replacedText = text.replaceAll("Hello|Hi", "こんにちは");

System.out.println(replacedText); // Output: こんにちは World! こんにちは there!
```

また、`replace()` メソッドでは大文字と小文字を区別しないオプションも指定することができます。次のコードでは、"hello" と "HELLO" を同じく "こんにちは" に置換しています。

```Java
String text = "Hello World. HELLO there!";
String replacedText = text.replaceAll("(?i)Hello", "こんにちは");

System.out.println(replacedText); // Output: こんにちは World. こんにちは there!
```

## 深堀り

上記のコードでは、`replaceAll()` メソッドを使用して複数の文字を一度に置換していますが、`replace()` メソッドを使用すると、1回の置換ごとにオリジナルの文字列がコピーされるため、パフォーマンスが低下します。そのため、複数の文字を置換する場合は `replaceAll()` メソッドを使用することが推奨されています。

また、正規表現を使用することで、より柔軟な検索と置換が可能になります。正規表現を学ぶことで、様々な文字列処理の場面で強力なツールとして活用することができます。

## さらに見る

- [JavaのStringクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [正規表現チュートリアル](https://www.tohoho-web.com/ex/regex.html)
- [Javaでの正規表現の使用方法について詳しく学ぶ](https://www.baeldung.com/java-regex)