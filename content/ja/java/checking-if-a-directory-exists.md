---
title:                "ディレクトリが存在するかどうかの確認"
date:                  2024-01-20T14:57:04.608807-07:00
html_title:           "Gleam: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
ディレクトリが存在するかどうかのチェックとは、指定されたパスにディレクトリが存在するか、どうかを判定する処理です。プログラマーはファイル操作を行う前に、エラーを避けるためにこのチェックをよく行います。

## How to:
Javaにおいて、ディレクトリが存在するかどうかは`Files.exists()`メソッドで簡単に確認できます。以下の例を見てみましょう。

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        Path path = Paths.get("/path/to/directory");

        if (Files.exists(path)) {
            System.out.println("ディレクトリが存在します。");
        } else {
            System.out.println("ディレクトリが存在しません。");
        }
    }
}
```

サンプル出力:

```
ディレクトリが存在します。
```
あるいは

```
ディレクトリが存在しません。
```

## Deep Dive
`Files.exists()`はJava 7で導入されたメソッドです。それ以前は`File`クラスの`exists()`メソッドを使っていましたが、新しい`java.nio.file`パッケージの方が柔軟でエラーに強くなっています。また、`Files.notExists()`メソッドはディレクトリが存在しないことを明示的に確認する際に使われ、三つ目の状態として「不明」(ディレクトリの存在が確認できない場合)も扱えます。なお、ディレクトリの存在確認はファイルシステムとのやり取りが発生するため、パフォーマンスへの影響を考慮する必要があります。

## See Also
- [Path Operations (Java Tutorial)](https://docs.oracle.com/javase/tutorial/essential/io/pathOps.html)
- [Class Files (Java API Documentation)](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [Class Path (Java API Documentation)](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Path.html)
