---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
simple_title:         "テキストファイルの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ?)
テキストファイルの書き込みとは、文字データをファイルに保存することだ。プログラマはデータの永続化やデータの共有、ログ作成のために行う。

## How to: (方法)
```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class WriteTextFileExample {
    public static void main(String[] args) {
        String text = "こんにちは、Java!";
        String fileName = "example.txt";

        // FileWriter と BufferedWriter を使う古い方法
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(fileName))) {
            writer.write(text);
        } catch (IOException e) {
            e.printStackTrace();
        }

        // Java NIO を使う新しい方法
        try {
            Files.writeString(Path.of(fileName), text);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
サンプルの出力：`example.txt` に "こんにちは、Java!" が書き込まれる。

## Deep Dive (掘り下げ)
テキストファイルの書き込みは、初期のプログラミング時代からある基本的な機能だ。`FileWriter`と`BufferedWriter`の組み合わせは歴史的によく使用されていたが、より最近では`java.nio.file.Files`の`writeString`メソッドのような新しい手法が推奨される。これはコードをシンプルにし、より読みやすくするためだ。また、文字エンコーディングの問題や、ファイルのロッキングといった実装の詳細も考慮する必要がある。

## See Also (関連情報)
- Java Documentation on Files.writeString: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#writeString(java.nio.file.Path,java.lang.CharSequence,java.nio.file.OpenOption...)
- Java I/O Guide by Oracle: https://docs.oracle.com/javase/tutorial/essential/io/
- BufferedWriter Documentation: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/BufferedWriter.html
- Java NIO Path Tutorial: https://www.baeldung.com/java-nio-2-file-api
