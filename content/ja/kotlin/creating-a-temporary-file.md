---
title:    "Kotlin: 一時ファイルを作成する"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成するのに理由はいくつかあります。一時的なデータを保存する必要がある、またはプログラムの実行中に一時的に生成されたファイルを処理する必要がある場合などです。

## 作り方

一時ファイルを作成するには、まずJava標準ライブラリの`java.io.File`をインポートする必要があります。その後、ファイルの名前と拡張子を指定してファイルを作成することができます。例えば、`temp.txt`という名前の一時ファイルを作成するには、次のように記述します。

```Kotlin
import java.io.File

val tempFile = File("temp.txt")
```

このようにすると、現在の作業ディレクトリに`temp.txt`というファイルが作成されます。また、ファイルの内容を指定したい場合には、`printWriter()`メソッドを使用することもできます。例えば、以下のようになります。

```Kotlin
import java.io.File

val tempFile = File("temp.txt")
tempFile.printWriter().use { out ->
    out.println("This is a temporary file.")
}
```

このコードでは、一時ファイルとして`temp.txt`が作成され、その中に`This is a temporary file.`という文字列が書き込まれます。

また、一時ファイルを作成する際には、`createTempFile()`メソッドを使用することもできます。このメソッドを使用すると、一時ファイルを作成した際に自動的にユニークな名前が付けられるため、同じ名前のファイルが作成される心配がありません。

```Kotlin
import java.io.File

val tempFile = File.createTempFile("temp", ".txt")
```

`temp`という名前のファイルが作成され、その後ろに自動的にユニークな文字列が付けられるため、ファイル名は`temp123456.txt`のようになります。

## 深掘り

一時ファイルを作成する際には、そのファイルを安全に削除する必要があります。Javaでは、`deleteOnExit()`メソッドを使用することで、プログラムが終了する際に自動的に一時ファイルが削除されるように設定することができます。ただし、このメソッドを使用するには、`createTempFile()`メソッドを使用してファイルを作成しなければなりません。

また、一時ファイルを作成する際にはファイルパスの指定が重要です。特にWindows環境では、バックスラッシュ`\`をエスケープ文字として使用するため、正しいファイルパスの指定が必要です。以下のように記述すると、例外が発生する可能性があります。

```Kotlin
val tempFile = File("C:\Users\temp.txt") // バックスラッシュをエスケープ文字として使用するため、正しくファイルパスが指定されていない
```

なので、ファイルパスを指定する際には、次のように記述する必要があります。

```Kotlin
val tempFile = File("C:\\Users\\temp.txt")
```

## 参考リンク

- [Temporary File Example in Kotlin](https://www.baeldung.com/java-temporary-files)
- [Kotlin Reference | java.io.File](https://kotlinlang.org/api