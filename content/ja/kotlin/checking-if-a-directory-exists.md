---
title:    "Kotlin: ディレクトリが存在するかどうかを確認する。"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜディレクトリの存在をチェックする必要があるのか

ディレクトリの存在を確認することは、ファイルを作成する前に既存のディレクトリを確認する必要がある場合に重要です。これにより、プログラムがエラーにならないようにすることができます。

## 方法

まず、プロジェクトで使用するKotlinファイルを作成します。次に、```File```クラスの```exists()```メソッドを使用して、ディレクトリの存在をチェックします。

```Kotlin
val directory = File("path/to/directory")

// ディレクトリが存在するかチェック
if (directory.exists()) {
    println("ディレクトリが既に存在します。")
} else {
    println("ディレクトリを作成します。")
    // ディレクトリを作成
    directory.mkdir()
}
```

上記のコードを実行すると、指定したパスにディレクトリが存在しない場合は新しく作成され、既に存在する場合はメッセージが表示されます。

## ディープダイブ

```exists()```メソッドは、ディレクトリの存在を確認するだけでなく、ファイルやシンボリックリンクの存在も確認することができます。また、プログラムで使用するパスは、絶対パスや相対パスの両方を指定することができます。

## 参考リンク

- [Kotlin Fileクラス公式ドキュメント](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Java Fileクラス公式ドキュメント](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Kotlin Fileクラスのメソッドを使用してディレクトリの作成と存在確認を行う](https://qiita.com/kojionilk/items/b3b63029379b2b05d17c)
- [Kotlinでディレクトリを作成する方法](https://www.it-swarm.dev/ja/kotlin/kotlindeskutopura-wotsukurumethodono-kara-anataha/b827144973cd65dcd996e2954fea8aa3/) 

## 関連リンク