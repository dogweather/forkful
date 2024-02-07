---
title:                "テキストファイルの読み込み"
date:                  2024-01-20T17:54:27.748112-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/reading-a-text-file.md"
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
