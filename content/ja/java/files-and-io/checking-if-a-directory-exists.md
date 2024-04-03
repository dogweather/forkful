---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:06.503011-07:00
description: "\u65B9\u6CD5\uFF1A Java\u3067\u306F\u3001\u30C7\u30A3\u30EC\u30AF\u30C8\
  \u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3092\u30C1\u30A7\u30C3\u30AF\u3059\u308B\
  \u3044\u304F\u3064\u304B\u306E\u65B9\u6CD5\u304C\u3042\u308A\u307E\u3059\u304C\u3001\
  \u4E3B\u306B`java.nio.file.Files`\u30AF\u30E9\u30B9\u3068`java.io.File`\u30AF\u30E9\
  \u30B9\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002 **`java.nio.file.Files`\u3092\u4F7F\
  \u7528\u3059\u308B**:\u2026"
lastmod: '2024-03-13T22:44:41.968254-06:00'
model: gpt-4-0125-preview
summary: "Java\u3067\u306F\u3001\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\
  \u3059\u308B\u304B\u3092\u30C1\u30A7\u30C3\u30AF\u3059\u308B\u3044\u304F\u3064\u304B\
  \u306E\u65B9\u6CD5\u304C\u3042\u308A\u307E\u3059\u304C\u3001\u4E3B\u306B`java.nio.file.Files`\u30AF\
  \u30E9\u30B9\u3068`java.io.File`\u30AF\u30E9\u30B9\u3092\u4F7F\u7528\u3057\u307E\
  \u3059."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

## 方法：
Javaでは、ディレクトリが存在するかをチェックするいくつかの方法がありますが、主に`java.nio.file.Files`クラスと`java.io.File`クラスを使用します。

**`java.nio.file.Files`を使用する**:

これは、最新のJavaバージョンで推奨されるアプローチです。

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // ここにディレクトリパスを指定
        String directoryPath = "path/to/directory";

        // ディレクトリが存在するかをチェックする
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("ディレクトリは存在します。");
        } else {
            System.out.println("ディレクトリは存在しません。");
        }
    }
}
```
**サンプル出力**:
```
ディレクトリは存在します。
```
または
```
ディレクトリは存在しません。
```

**`java.io.File`を使用する**:

`java.nio.file.Files`が推奨される一方で、古い`java.io.File`クラスも使用可能です。

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // ここにディレクトリパスを指定
        String directoryPath = "path/to/directory";

        // Fileオブジェクトを作成
        File directory = new File(directoryPath);

        // ディレクトリが存在するかをチェックする
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("ディレクトリは存在します。");
        } else {
            System.out.println("ディレクトリは存在しません。");
        }
    }
}
```
**サンプル出力**:
```
ディレクトリは存在します。
```
または
```
ディレクトリは存在しません。
```

**サードパーティライブラリを使用する**:

標準Javaライブラリで通常はこのタスクに十分ですが、Apache Commons IOのようなサードパーティライブラリは、より複雑なアプリケーションで役立つ追加のファイル処理ユーティリティを提供します。

**Apache Commons IO**:

まず、プロジェクトにApache Commons IOの依存性を追加します。その後、ディレクトリの存在をチェックするためにその機能を使用できます。

```java
// Apache Commons IOがプロジェクトに追加されていると仮定

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // ここにディレクトリパスを指定
        String directoryPath = "path/to/directory";

        // FileUtilsを使用してチェックする
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("ディレクトリは存在します。");
        } else {
            System.out.println("ディレクトリは存在しません。");
        }
    }
}
```

**注意**: `FileUtils.directoryContains`は特定のファイルがディレクトリに含まれているかどうかをチェックしますが、第二引数に`null`を渡すことにより、ディレクトリの存在をチェックするために使用できます。ただし、これは最も直接的または本来の使用方法ではないかもしれないので注意してください。

**サンプル出力**:
```
ディレクトリは存在します。
```
または
```
ディレクトリは存在しません。
```
