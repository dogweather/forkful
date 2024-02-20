---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:29.784813-07:00
description: "Java\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\
  \u304F\u3068\u3044\u3046\u306E\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\
  \u30E0\u4E0A\u306E\u30D5\u30A1\u30A4\u30EB\u306B\u5185\u5BB9\u3092\u4F5C\u6210\u3057\
  \u3001\u66F8\u304D\u8FBC\u3080\u305F\u3081\u306B\u8A00\u8A9E\u306E\u6A5F\u80FD\u3092\
  \u4F7F\u7528\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30ED\u30B0\u306E\u8A18\u9332\u3001\u30C7\
  \u30FC\u30BF\u306E\u30A8\u30AF\u30B9\u30DD\u30FC\u30C8\u3001\u307E\u305F\u306F\u5F8C\
  \u3067\u53D6\u5F97\u3059\u308B\u305F\u3081\u306E\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u72B6\u614B\u306E\u4FDD\u5B58\u306A\u3069\u3001\u3055\u307E\u3056\u307E\
  \u306A\u7406\u7531\u3067\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.133271
model: gpt-4-0125-preview
summary: "Java\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\u304F\
  \u3068\u3044\u3046\u306E\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\
  \u4E0A\u306E\u30D5\u30A1\u30A4\u30EB\u306B\u5185\u5BB9\u3092\u4F5C\u6210\u3057\u3001\
  \u66F8\u304D\u8FBC\u3080\u305F\u3081\u306B\u8A00\u8A9E\u306E\u6A5F\u80FD\u3092\u4F7F\
  \u7528\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30ED\u30B0\u306E\u8A18\u9332\u3001\u30C7\u30FC\
  \u30BF\u306E\u30A8\u30AF\u30B9\u30DD\u30FC\u30C8\u3001\u307E\u305F\u306F\u5F8C\u3067\
  \u53D6\u5F97\u3059\u308B\u305F\u3081\u306E\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u72B6\u614B\u306E\u4FDD\u5B58\u306A\u3069\u3001\u3055\u307E\u3056\u307E\u306A\
  \u7406\u7531\u3067\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？

Javaでテキストファイルを書くというのは、ファイルシステム上のファイルに内容を作成し、書き込むために言語の機能を使用することを意味します。プログラマーは、ログの記録、データのエクスポート、または後で取得するためのアプリケーション状態の保存など、さまざまな理由でこれを行います。

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
