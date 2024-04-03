---
date: 2024-01-20 17:40:46.382660-07:00
description: "How to: (\u5B9F\u88C5\u65B9\u6CD5) Java\u3067\u306F\u3001`java.nio.file`\u30D1\
  \u30C3\u30B1\u30FC\u30B8\u306E`Files`\u30AF\u30E9\u30B9\u3092\u4F7F\u3063\u3066\u7C21\
  \u5358\u306B\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3067\u304D\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.975829-06:00'
model: gpt-4-1106-preview
summary: "Java\u3067\u306F\u3001`java.nio.file`\u30D1\u30C3\u30B1\u30FC\u30B8\u306E\
  `Files`\u30AF\u30E9\u30B9\u3092\u4F7F\u3063\u3066\u7C21\u5358\u306B\u4E00\u6642\u30D5\
  \u30A1\u30A4\u30EB\u3092\u4F5C\u6210\u3067\u304D\u307E\u3059."
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

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
