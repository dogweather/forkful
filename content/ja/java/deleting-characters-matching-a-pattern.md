---
title:                "パターンにマッチする文字を削除する"
html_title:           "Java: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

＃＃ 何ですか？
文字に一致するキャラクターを削除するとは、特定のパターンに一致する文字をプログラムから削除することです。プログラマーがこれを行う理由は、プログラムをより効率的にするためです。

＃＃ 方法：
```Java
文字列 str = "Hello World";
文字列のキャラクターを削除するパターン= "l";
str = str.replaceAll (pattern, "");
System.out.println（ストリング）; 
```
出力： "Heo Word"

＃＃ 深く潜る
（1）歴史的背景：文字を削除するというアイデアは、古代からプログラミングが始まった頃から存在しています。しかし、現在のJavaでは、より効率的な方法が開発されました。
（2）代替手段：例えば、正規表現を使用することにより、より一般的な文字列の操作が可能になります。
（3）実装の詳細：Javaはすでに文字を削除するための組み込みメソッドを提供していますが、より複雑なパターンにマッチする文字を削除する場合は、正規表現を使用してカスタマイズする必要があります。

＃＃ 参考
- Java APIドキュメント：https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-
- 正規表現入門：https://www.tohoho-web.com/js/regexp.htm