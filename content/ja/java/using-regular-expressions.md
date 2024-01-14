---
title:    "Java: 正規表現の使用"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使うのはなぜでしょうか？Javaプログラマーにとって、正規表現は非常に強力なツールです。例えば、文字列のパターンマッチングや置換を行う場合に役立ちます。また、データの抽出や検証など、様々なシーンで活用することができます。

## 方法

正規表現を使用するには、まずはjava.util.regexパッケージをインポートする必要があります。このパッケージには、PatternとMatcherという2つのクラスがあります。

例えば、以下のようにパターンを定義し、マッチさせる文字列を指定します。

```Java
Pattern pattern = Pattern.compile("[A-Za-z]+"); //英字のみをマッチさせるパターン
String text = "Regular Expressions are powerful"; //検索対象の文字列
```

次に、Matcherを使用してマッチングを行います。

```Java
Matcher matcher = pattern.matcher(text);
while (matcher.find()) {
    System.out.println(matcher.group()); //マッチした文字列を出力
}
```

上記の場合、出力結果は「Regular」「Expressions」「are」「powerful」になります。

## 深堀り

正規表現には、様々なメタキャラクターが存在します。例えば、「. (ドット)」は任意の1文字を表し、「* (アスタリスク)」は直前の文字の0回以上の繰り返しを表します。これらのメタキャラクターを組み合わせることで、より複雑な文字列のマッチングを行うことができます。

また、正規表現はグループ化することも可能です。グループ化すると、マッチした文字列を後で参照することができます。例えば、「([A-Z]+) ([A-Z]+)」という正規表現で、「Bob Smith」をマッチさせた場合、group(1)は「Bob」を、group(2)は「Smith」を表します。

さらに、正規表現のパフォーマンスを向上させるための最適化の方法もあります。例えば、不必要なバックトラッキングを避けることで、パフォーマンスを向上させることができます。

## 参考リンク

- [java.util.regex パッケージドキュメント (Oracle)](https://docs.oracle.com/javase/8/docs/api/java/util/regex/package-summary.html)
- [Java 正規表現チュートリアル (RegexOne)](https://regexone.com/references/java)
- [正規表現を使いこなそう (Qiita)](https://qiita.com/kwst/items/f861c5c08a01f7279b39)