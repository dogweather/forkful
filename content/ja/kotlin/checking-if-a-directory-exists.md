---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "C#: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

---

## 何となぜ?（What & Why?）

ディレクトリ存在確認とは、特定のディレクトリが存在するかどうかを確かめるプログラミング処理のことです。これを行う理由は、プログラムがファイルにアクセスする前に、ディレクトリが存在するか否かを確認し、エラーを防ぐためです。

---

## 手順（How to）

以下のコードを使用してKotlinでディレクトリの存在を確認できます。

```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val directory = Paths.get("ディレクトリパス")

    if (Files.exists(directory)) {
        println("ディレクトリが存在します")
    } else {
        println("ディレクトリが存在しません")
    }
}
```

これが出力結果です。

```Kotlin
ディレクトリが存在します
// または
ディレクトリが存在しません
```

---

## 詳細（Deep Dive） 

ディレクトリ存在確認は古くからある重要な操作です。これにより、プログラムが途中でエラーに陥ることなく、スムーズに実行を続けることができます。

代替手段としては、KotlinでdeprecatedとなったJavaのFileクラスを使う方法があります。しかしこの方法は冗長であり、新しいNIOを使う方法が推奨されています。

ディレクトリ存在確認の実装については、基本的にはファイルシステムに対して指定のパスのディレクトリが存在するか確認するクエリを実行します。

---

## 参照（See Also）

以下のリンクで関連情報を見つけることができます。

- [Kotlin公式ドキュメンテーション](https://kotlinlang.org/docs/reference/)
- [Java NIO.2公式チュートリアル（英語）](https://docs.oracle.com/javase/tutorial/essential/io/pathOps.html)
- [Java Fileクラスについて（英語）](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)