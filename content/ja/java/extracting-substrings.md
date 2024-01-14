---
title:                "Java: 部分文字列の抽出"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/extracting-substrings.md"
---

{{< edit_this_page >}}

##なぜ

文字列から部分文字列を抽出する必要性は、プログラミングにおいて非常に一般的です。例えば、テキスト処理やユーザー入力の検証など、さまざまなシナリオで部分文字列を抽出する必要があります。部分文字列を抽出することで、より効率的かつ正確なプログラムを作成することができるようになります。

##方法

抽出したい部分文字列の開始位置と長さを指定することで、Javaのsubstringメソッドを使用して簡単に部分文字列を抽出することができます。以下の例では、"Hello World!"という文字列から、"World"という部分文字列を抽出しています。

```Java
String str = "Hello World!";
String subStr = str.substring(6, 11);
System.out.println(subStr); // Output: World
```

もし部分文字列の開始位置を指定せず、終了位置だけを指定したい場合は、文字列の長さを取得し、そこから計算して部分文字列を抽出することもできます。

```Java
String str = "Hello World!";
int end = str.length();
String subStr = str.substring(end - 5, end);
System.out.println(subStr); // Output: World
```

##ディープダイブ

Javaのsubstringメソッドは、内部的には開始位置と長さを指定する別の方法のオーバーロードメソッドを使用しています。これにより、より複雑な部分文字列の抽出も可能になります。また、Unicode文字や特殊文字にも対応しています。これらの詳細については、Javaの公式ドキュメントを参照することをお勧めします。

##関連リンク

- [Javaのsubstringメソッドのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/lang/String.html#substring-int-int-)
- [JavaのStringクラスのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/lang/String.html)