---
title:    "Kotlin: ディレクトリが存在するかどうかをチェックする"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかをチェックすることの重要性は多岐にわたります。例えば、ファイルを読み書きする時や、バックアッププログラムを実行する前にディレクトリが存在するかを確認するなど、プログラムの実行において必要不可欠な要素です。

## 方法

ディレクトリが存在するかどうかをチェックするには、 `File` クラスの `exists()` メソッドを使用します。以下の例を参考にしてください。

```Kotlin
val directory = File("/path/to/directory")

if(directory.exists()){
    println("ディレクトリが存在します。")
} else {
    println("ディレクトリが存在しません。")
}
```

上記のコードでは、まず `File` クラスのインスタンスを作成し、引数としてチェックしたいディレクトリのパスを指定します。次に、 `exists()` メソッドを使用してディレクトリの存在をチェックし、存在する場合には "ディレクトリが存在します。" というメッセージを出力します。

## ディープダイブ

`exists()` メソッドは、指定したパスにファイルやディレクトリが存在するかどうかを真偽値で返します。ただし、このメソッドはディレクトリの存在だけをチェックするため、サブディレクトリの存在まではチェックしません。また、実際にファイルやディレクトリが存在するかどうかはファイルシステムのパーミッションにも左右されます。

## 参考リンク

[JavaDoc: File.exists()](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--)
[Kotlin 公式ドキュメント: File.exists()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/exists.html)
[GitHub: checking directory existence in Kotlin](https://github.com/KotlinBy/awesome-kotlin/blob/master/examples/checking_directory_existence.kt)

## 参考