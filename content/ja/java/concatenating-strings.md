---
title:    "Java: 文字列の連結"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を連結させることに関わる理由について説明します。Javaのプログラミングにおいて、文字列の連結は非常に重要な操作です。複数の文字列を一つに合体させることで、より複雑で役立つプログラムを作ることができます。また、出力したい文字列の形式を自由に設定することができます。

## 方法

文字列を連結させるためには、文字列を加算演算子（+）で結合することができます。例えば、"Hello"と"World"を連結させる場合は次のように記述します。

```Java
String str1 = "Hello";
String str2 = "World";

String result = str1 + str2;

System.out.println(result); //output: HelloWorld
```

複数の文字列を連結させる場合は、文字列変数を区切り記号として文字列を加えたり、文字列ライブラリのメソッドを使用することができます。以下は、複数の文字列をスペースで区切って連結させる例です。

```Java
String str1 = "Hello";
String str2 = "World";
String str3 = "from";
String str4 = "Java";

String result = str1 + " " + str2 + " " + str3 + " " + str4;

System.out.println(result); //output: Hello World from Java
```

## 深堀り

文字列を連結させる際に気をつける点として、文字列の連結回数や位置などによってパフォーマンスに影響があることが挙げられます。文字列変数に代入する求める文字列の数が少ない場合は、加算演算子を使用するのが適しています。しかし、大量の文字列を連結させる場合は、文字列ライブラリのStringBuilderクラスを使用することが推奨されています。StringBuilderクラスは、文字列を可変で格納することによってパフォーマンスを向上させることができます。

## その他

もし文字列の連結についてもっと深く学びたい場合は、以下のリンクを参考にしてください。

見出し：参考リンク

- [Java公式ドキュメント](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#concat-java.lang.String-)
- [String, StringBuffer, StringBuilderの違い](https://zero5eight9.com/java-string/#i-3)
- [Javaにおける文字列の扱い方](https://qiita.com/AkihiroTakamura/items/9a8ab575f16365700f88)