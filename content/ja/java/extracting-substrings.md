---
title:                "Java: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

サブストリングを抽出することには、多くの理由があります。例えば、テキスト処理や文字列操作を行う際に、特定の部分だけを抽出したい場合があります。また、文字列を比較する際にも、サブストリングを使用することができます。簡単に言えば、サブストリングを抽出することは、プログラミングの便利なテクニックのひとつです。

## 抽出方法

サブストリングを抽出する方法は、非常に簡単です。まずは、抽出したい部分の開始位置と終了位置を指定します。例えば、文字列の最初から5文字目までを抽出する場合は、「substring(0,5)」のように指定します。続いて、そのサブストリングを変数に代入することで、後の処理に使うことができます。以下は、Javaでサブストリングを抽出する例です。

```Java
String text = "今日はとても暑いです";
String substring = text.substring(2, 5);
System.out.println(substring);  // 出力結果：「とても」
```

また、終了位置を省略することで、文字列の最後までを抽出することもできます。例えば、「substring(2)」と指定した場合は、文字列の最後までを抽出します。

## 深堀り

サブストリングを抽出する際には、いくつかの注意点があります。まず、開始位置を指定する際は、インデックス番号を使用します。つまり、最初の文字は「0」から始まります。また、終了位置は開始位置よりも1つ大きい数字を指定する必要があります。さらに、文字列の長さを超える数字を指定した場合は、エラーが発生します。これらの点に注意して、サブストリングを使用するようにしましょう。

## 参考リンク

[Javaで文字列の操作をする方法](https://www.javadrive.jp/start/string/index1.html)
[TechAcademy - Javaで文字列を抽出する方法](https://techacademy.jp/magazine/47006)
[Java APIドキュメント - substringメソッド](https://docs.oracle.com/javase/jp/6/api/java/lang/String.html#substring(int,%20int))