---
title:    "Java: 「文字列を小文字に変換する」"
keywords: ["Java"]
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換する理由は、文字列の比較や検索をより簡単にするためです。また、データの整理や処理を行う際にも重要な役割を果たします。

## 方法

文字列を小文字に変換するには、Javaの標準ライブラリであるStringクラスのtoLowerCase()メソッドを使用します。以下のコードブロックを参考にしてください。

```Java
String str = "HELLO WORLD";
String lowerCaseStr = str.toLowerCase();
System.out.println(lowerCaseStr);
```

実行結果は以下の通りになります。

```
hello world
```

## ディープダイブ 

文字列を小文字に変換する際には、ロケール（地域や言語の設定）によって結果が異なることがあります。デフォルトでは、StringクラスのtoLowerCase()メソッドは現在のロケールを使用して変換を行いますが、ロケールを指定することで特定の言語や地域の基準に合わせた変換が可能です。

また、文字列の大文字や小文字を無視して比較を行う際にもtoLowerCase()メソッドは重要な役割を果たします。これにより、より柔軟な文字列比較が可能になります。

## See Also

- [Java Stringクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Localeクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)
- [Javaでの文字列比較の方法について](https://dev.classmethod.jp/articles/java-string-comparison/)