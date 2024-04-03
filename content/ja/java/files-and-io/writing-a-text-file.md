---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:29.784813-07:00
description: "\u65B9\u6CD5: #."
lastmod: '2024-03-13T22:44:41.974452-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## 方法:


### `java.nio.file`を使用する（標準ライブラリ）
JavaのNew I/O（NIO）パッケージ（`java.nio.file`）は、ファイルを扱うためのより多様なアプローチを提供します。これは`Files.write()`を使用してファイルに書き込む簡単な方法です：

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lines = Arrays.asList("Line 1", "Line 2", "Line 3");
        try {
            Files.write(Paths.get("example.txt"), lines);
            System.out.println("ファイルの書き込みに成功しました！");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

出力：

```
ファイルの書き込みに成功しました！
```

### `java.io`を使用する（標準ライブラリ）
より伝統的なアプローチには、`java.io.FileWriter`がテキストファイルを単純に書き込むための良い選択です：

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("example.txt")) {
            writer.write("Hello, World!\n");
            writer.append("これは別の行です。");
            System.out.println("ファイルの書き込みに成功しました！");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

出力：

```
ファイルの書き込みに成功しました！
```

### Apache Commons IOを使用する
Apache Commons IOライブラリは、ファイルの書き込みを含む多くの操作を簡素化します。次のように`FileUtils.writeStringToFile()`を使用してファイルに書き込みます：

まず、プロジェクトに依存関係を追加します。Mavenを使用している場合は、以下を含めます：

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- 最新バージョンを確認してください -->
</dependency>
```

次に、次のコードを使用してファイルにテキストを書き込みます：

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("example.txt"), "これはCommons IOを使用して書かれたテキストです。", "UTF-8");
            System.out.println("ファイルの書き込みに成功しました！");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

出力：

```
ファイルの書き込みに成功しました！
```
