---
title:    "Java: 文字列の抽出"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の一部を抽出することは、プログラミングにおいて非常に便利な機能です。例えば、ある文字列から特定の文字を検索したり、文字列を分割したりする際に使うことができます。また、プレゼンテーションやデータのフォーマットを整える際にも役立ちます。この記事では、Javaで文字列を抽出する方法について説明します。

## 方法

Javaでは、文字列を抽出するためのいくつかの方法があります。まずは、substringメソッドを使用します。このメソッドを使うことで、文字列から指定した位置の文字を抽出することができます。

一つ目の例として、次のような文字列があるとします。

```
String str = "こんにちは、世界！";
```

ここで、 "「こんにちは」"という部分を抽出したいとします。この場合、substringメソッドを使うと以下のようになります。

```
String greeting = str.substring(0, 5);
System.out.println(greeting);
```

上記のコードで出力されるのは、「こんにちは」という文字列になります。

また、substringメソッドは文字列の中に含まれる文字を指定した位置から抽出することもできます。例えば、次のような文字列があるとします。

```
String url = "https://www.example.com";
```

この場合、ドメイン名の部分だけを抽出したいとします。その場合、以下のようにコードを書くことができます。

```
String domain = url.substring(8, 22);
System.out.println(domain);
```

このコードでは、 "www.example.com"という部分が抽出されます。

## 深く掘り下げる

substringメソッド以外にも、文字列を抽出するための方法があります。例えば、正規表現を使うことで、特定のパターンにマッチする文字列を抽出することができます。これは、特定の形式で入力されたデータから必要な情報を抽出する際に役立ちます。

また、文字列を分割するためのsplitメソッドや、特定の文字を検索した後にその文字の位置を取得するためのindexOfメソッドもあります。これらの方法を使うことで、より柔軟に文字列を抽出することができます。

## 参考リンク

- [Javaで文字列を分割・抽出する方法](https://qiita.com/yshi12/items/8955ba2edae0e1116795)
- [Java Stringの抽出、削除、変換まとめ](https://qiita.com/opengl-8080/items/dbd4432fd6eabb246def)
- [Javaの正規表現で文字列を抽出する方法](https://qiita.com/meguroman/items/28faa8321202e53fbe3f)

## みんなのHelloWorld

```
public class HelloWorld {

    public static void main(String[] args) {

        String str = "Hello, world!";
        String hello = str.substring(0, 5);
        System.out.println(hello);
    }
}
```

出力結果：Hello

## See Also

- [Java Stringクラスのsubstringメソッドの使い方](https://www.javadrive.jp/start/string/index5.html)
- [Java正規表現チートシート](http://docs.oracle.com/javase/jp/7/api/java/util/regex/Pattern.html)