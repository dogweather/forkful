---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ？(What & Why?)

文字列の連結は、2つ以上の文字列をひとつにまとめるプロセスです。プログラマーにとって、これはデータを加工したり操作したりするための効率的な手段となります。

## 手順 (How to)

Javaで文字列を連結する一番基本的な方法は、`+` (プラス) 演算子を使用することです。

```Java
String hello = "こんにちは, ";
String world = "世界!";
String greeting = hello + world;
System.out.println(greeting);
```

このプログラムは、`こんにちは, 世界!`と出力します。

## 深掘り (Deep Dive)

Java初期のバージョンでは、`+` 演算子が文字列連結のための唯一の方法でした。しかし、大量の文字列連結操作を行う際には、効率的ではありませんでした。これは、文字列がイミュータブル（変更不可能）であるため、それぞれの連結操作で新しい文字列が生成されるためです。

それを解決するための代替手段として、`StringBuilder`と`StringBuffer`があります。これらはミュータブル（変更可能）なオブジェクトで、文字列を効率的に連結します。

```Java
StringBuilder sb = new StringBuilder("こんにちは, ");
sb.append("世界!");
System.out.println(sb.toString());
```

このコードも、`こんにちは, 世界!`と出力します。しかし、`StringBuilder`を使用すると、新しい文字列インスタンスを生成せずに文字列を連結できます。

## 参考資料 (See Also)

- [Javaの文字列クラスのドキュメンテーション](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [StringBuilderクラスのドキュメンテーション](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [Oracleのチュートリアル: 文字列を連結する](https://docs.oracle.com/javase/tutorial/java/data/buffers.html)