---
title:                "標準エラーへの書き込み"
date:                  2024-01-19
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"

category:             "Java"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
標準エラーへの書き込みは、エラーメッセージやデバッグ情報を出力するために使用されます。これにより、正常な出力とは別にエラー内容を簡単に確認できるようになります。

## How to:
```java
public class StdErrExample {
    public static void main(String[] args) {
        System.out.println("これは標準出力です。");
        System.err.println("これは標準エラーです。");
    }
}
```

出力例:
```
これは標準出力です。
これは標準エラーです。
```

## Deep Dive
標準エラー出力(`System.err`)はUNIXの伝統からきています。`System.out`とは異なり、通常はバッファリングされず、即座に表示されます。代替手段として、ログフレームワーク（例：Log4jやSLF4J）を使うこともできますが、シンプルなスクリプトや小規模なアプリケーションでの迅速なデバッグには`System.err`が便利です。内部的には`PrintStream`クラスが利用されています。

## See Also
- [System クラスのドキュメント](https://docs.oracle.com/javase/10/docs/api/java/lang/System.html)
- [PrintStream クラスのドキュメント](https://docs.oracle.com/javase/10/docs/api/java/io/PrintStream.html)
- [Log4j - Apache Logging Services](https://logging.apache.org/log4j/2.x/)
- [The Simple Logging Facade for Java (SLF4J)](http://www.slf4j.org/)
