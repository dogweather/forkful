---
title:                "ディレクトリが存在するかどうかの確認"
date:                  2024-01-20T14:57:28.212098-07:00
simple_title:         "ディレクトリが存在するかどうかの確認"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
ディレクトリが存在するかどうかをチェックするとは、ファイルパスが示す場所にフォルダがあるかを調べることです。このチェックは、ファイル操作前に誤ったエラーを防ぐため、または動的なパス作成の際に確実性を持たせるために行います。

## How to: (方法)
```kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val path = Paths.get("/path/to/directory")
    
    if (Files.exists(path)) {
        println("ディレクトリが存在します。")
    } else {
        println("ディレクトリが存在しません。")
    }
}
```

出力例:
```
ディレクトリが存在します。
```
もしくは
```
ディレクトリが存在しません。
```

## Deep Dive (詳細情報)
ディレクトリの存在チェックはファイルI/O処理の基本で、Javaの標準ライブラリから継承された `java.nio.file.Files` クラスの `exists` 方法をKotlinでも利用できます。代替手段として `File` クラスを使う方法もありますが、`Files` クラスのほうが新しく、より多機能です。実装の際には、シンボリックリンクが指し示す先の存在をチェックするか否か (`NOFOLLOW_LINKS` オプション) といった詳細な制御が可能です。

## See Also (関連情報)
- [java.nio.file.Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [Kotlinの公式ドキュメント](https://kotlinlang.org/docs/home.html)
