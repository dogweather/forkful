---
title:                "文字列の大文字化"
html_title:           "Java: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字にすることに興味がある人々にとって、この記事は役立つ情報を提供します。文字列を大文字にすることは、データ処理やUIデザインなど、多くのプログラミングタスクで必要になるためです。

## 方法

```Java
public class StringCapitalization {
    public static void main(String[] args) {
        // 入力の文字列
        String input = "hello world";
        // 文字列を大文字に変換
        String capitalized = input.toUpperCase();
        // 変換した文字列の出力
        System.out.println(capitalized);
    }
}
```
```
HELLO WORLD
```

この例では、Javaの`toUpperCase()`メソッドを使用して文字列を大文字に変換しています。このメソッドは、文字列の各文字を大文字に変換し、新しい文字列オブジェクトとして返します。単純な方法で文字列を大文字に変換することができます。

## ディープダイブ

文字列を大文字に変換する際には、`toUpperCase()`メソッドの他にもさまざまな方法があります。例えば、Javaの`String`クラスには、`charAt()`メソッドを使用して文字列の各文字にアクセスし、`Character`クラスの`toUpperCase()`メソッドを使用して各文字を大文字に変換することができます。また、正規表現を使用して文字列内の特定の文字を大文字に変換することもできます。さらに、Javaの`StringBuilder`クラスを使用して文字列を操作する方法もあります。それぞれの方法については、関連リンクを参照してください。

## 関連リンク

- [Java StringクラスのtoUpperCase()メソッドの仕様 (英語)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase--)
- [Java CharacterクラスのtoUpperCase()メソッドの仕様 (英語)](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html#toUpperCase-char-)
- [Java正規表現チュートリアル (日本語)](https://java.keicode.com/lang/regex-pattern.php)
- [Java StringBuilderクラスの使い方 (日本語)](https://www.javadrive.jp/start/stringbuilder/index1.html)