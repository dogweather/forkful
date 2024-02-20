---
date: 2024-01-20 17:54:27.748112-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\
  \u307F\u306F\u3001\u305D\u306E\u5185\u5BB9\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\
  \u4F7F\u3046\u305F\u3081\u306E\u64CD\u4F5C\u3067\u3059\u3002\u306A\u305C\u5FC5\u8981\
  \u304B\uFF1F\u60C5\u5831\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\
  \u3092\u6271\u3063\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u304C\u3088\u304F\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.131695
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\
  \u307F\u306F\u3001\u305D\u306E\u5185\u5BB9\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\
  \u4F7F\u3046\u305F\u3081\u306E\u64CD\u4F5C\u3067\u3059\u3002\u306A\u305C\u5FC5\u8981\
  \u304B\uFF1F\u60C5\u5831\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\
  \u3092\u6271\u3063\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u304C\u3088\u304F\u4F7F\u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
テキストファイルの読み込みは、その内容をプログラムで使うための操作です。なぜ必要か？情報を取得したり、データを扱ったりするためにプログラマーがよく使います。

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
