---
title:                "文字列の補間"
html_title:           "Java: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何をするのか？なぜするのか？
文字列を内挿することとは何か、そしてなぜプログラマーがそれを行うのかを説明します。

文字列の内挿とは、既存の文字列に別の値を埋め込むことを指します。プログラマーが文字列を内挿する理由は、可読性を向上させたり、ダイナミックな文字列を作成するためです。

例えば、「こんにちは、私の名前はLauraです。」という文字列があったとします。ここに "Laura" の代わりに別の名前を埋め込むことで、同じコードを使用して異なる名前を持つさまざまなメッセージを作成することができます。

## 手順：
下に例を示し、文字列を内挿する方法を説明します。

```Java
String name = "Laura";
System.out.println("こんにちは、私の名前は" + name + "です。");
```

このコードの出力は次のようになります：
```
こんにちは、私の名前はLauraです。
```

## 深堀り
文字列の内挿は、テンプレートエンジンとして知られるフレームワークを使用することによっても行うことができます。テンプレートエンジンでは、コードの外部にテンプレートと呼ばれるファイルを作成し、そこに必要な変数を埋め込むことができます。

また、文字列内挿のほかにも、String.format()メソッドを使用して文字列をフォーマットすることもできます。この方法を使用すると、より柔軟に文字列を操作することができます。

## 関連情報
- [Java Docs: String.format method](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#format-java.lang.String-java.lang.Object...-)
- [Thymeleaf - Java template engine](https://www.thymeleaf.org/)