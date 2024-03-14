---
date: 2024-01-20 17:40:46.382660-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30E0\u4E2D\u3067\u4E00\u6642\u30D5\u30A1\u30A4\
  \u30EB\u3092\u4F5C\u308B\u3053\u3068\u306F\u3001\u30C7\u30FC\u30BF\u3092\u77ED\u671F\
  \u9593\u4FDD\u5B58\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u4E00\u6642\u7684\u306A\
  \u30C7\u30FC\u30BF\u3092\u30C7\u30A3\u30B9\u30AF\u306B\u6301\u305F\u305B\u305F\u3044\
  \u6642\u3084\u3001\u5927\u304D\u306A\u30C7\u30FC\u30BF\u64CD\u4F5C\u3092\u884C\u3046\
  \u4E2D\u9593\u30B9\u30C6\u30C3\u30D7\u304C\u5FC5\u8981\u306A\u6642\u306B\u4F7F\u3044\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.975829-06:00'
model: gpt-4-1106-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30E0\u4E2D\u3067\u4E00\u6642\u30D5\u30A1\u30A4\
  \u30EB\u3092\u4F5C\u308B\u3053\u3068\u306F\u3001\u30C7\u30FC\u30BF\u3092\u77ED\u671F\
  \u9593\u4FDD\u5B58\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u4E00\u6642\u7684\u306A\
  \u30C7\u30FC\u30BF\u3092\u30C7\u30A3\u30B9\u30AF\u306B\u6301\u305F\u305B\u305F\u3044\
  \u6642\u3084\u3001\u5927\u304D\u306A\u30C7\u30FC\u30BF\u64CD\u4F5C\u3092\u884C\u3046\
  \u4E2D\u9593\u30B9\u30C6\u30C3\u30D7\u304C\u5FC5\u8981\u306A\u6642\u306B\u4F7F\u3044\
  \u307E\u3059\u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
プログラム中で一時ファイルを作ることは、データを短期間保存する方法です。一時的なデータをディスクに持たせたい時や、大きなデータ操作を行う中間ステップが必要な時に使います。

## How to: (実装方法)
Javaでは、`java.nio.file`パッケージの`Files`クラスを使って簡単に一時ファイルを作成できます。

```java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            // 一時ファイルの作成
            Path tempFile = Files.createTempFile(null, ".tmp");
            System.out.println("Temporary file created: " + tempFile);

            // ファイルに何かしらを書き込む (例えば "Hello, World!")
            Files.writeString(tempFile, "Hello, World!");

            // 一時ファイルの内容を読み込む
            String content = Files.readString(tempFile);
            System.out.println("File content: " + content);

            // 必要な処理が終わったら一時ファイルを削除
            Files.delete(tempFile);
            System.out.println("Temporary file deleted.");

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

サンプル出力:
```
Temporary file created: /tmp/1234567890.tmp
File content: Hello, World!
Temporary file deleted.
```

## Deep Dive (深掘り)
`Files.createTempFile`はJava 7で導入されて以来、一時ファイル作成の標準的な方法です。古い`java.io.File`にも似た機能がありますが非推奨。`Files.createTempFile`は使いやすく、安全な一時ファイルを提供します。処理が終わったら一時ファイルは削除すべきですが、プログラムが予期せず終了した場合でもOSがそれをクリーンアップすることが多いです。

## See Also (関連情報)
- [`Files.createTempFile`のJavaDoc](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#createTempFile(java.nio.file.Path,java.lang.String,java.lang.String,java.nio.file.attribute.FileAttribute...))
- [Java NIOファイルAPIガイド](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
- [`java.io.File`についての非推奨の情報](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/File.html#%3Cinit%3E(java.lang.String))
