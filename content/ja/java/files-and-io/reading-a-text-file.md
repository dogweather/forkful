---
date: 2024-01-20 17:54:27.748112-07:00
description: "How to: (\u3084\u308A\u65B9) Java\u3067\u306F`Files`\u3068`Paths`\u30AF\
  \u30E9\u30B9\u3067\u7C21\u5358\u306B\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\
  \u3092\u8AAD\u3080\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.972931-06:00'
model: gpt-4-1106-preview
summary: "Java\u3067\u306F`Files`\u3068`Paths`\u30AF\u30E9\u30B9\u3067\u7C21\u5358\
  \u306B\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

## How to: (やり方)
Javaでは`Files`と`Paths`クラスで簡単にテキストファイルを読むことができます。

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.io.IOException;

public class ReadTextFile {
    public static void main(String[] args) {
        String path = "sample.txt"; // 読むファイルのパス

        try {
            String content = new String(Files.readAllBytes(Paths.get(path)));
            System.out.println(content);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

`sample.txt`の内容をコンソールに表示します。

## Deep Dive (深掘り)
Java では昔から`FileInputStream`, `BufferedReader`などでファイルを読んできましたが、Java 7から`java.nio.file`パッケージが導入され、ファイルI/Oが簡単に、そしてより効率的に行えるようになりました。`Files`クラスは、このパッケージの一つで、ファイルの読み書きをシンプルなメソッド呼び出しで行える便利なユーティリティを提供します。

他の方法としては、`Scanner` クラスを使用することもできますが、これは解析が必要なときや小さいファイルに適していることが多いです。実装の詳細については、ストリームの使用、エンコーディングの取り扱い、例外の処理などがありますが、これはコンテキストやニーズに応じて変わります。

## See Also (関連情報)
- [`Files`クラスドキュメント](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html)
- [`Paths`クラスドキュメント](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Paths.html)
- [Java I/Oチュートリアル](https://docs.oracle.com/javase/tutorial/essential/io/)
- [`BufferedReader`クラスの使用例](https://www.baeldung.com/java-buffered-reader)
