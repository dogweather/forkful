---
title:    "Kotlin: テキストファイルの読み込み"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## なぜ
テキストファイルを読むことの重要性について説明します。テキストファイルは、プログラマーにとって非常に有用であり、コードを保存し、読み書きする手段として使われます。このブログ記事では、Kotlinを使用してテキストファイルをどのように読むかを紹介します。

## 方法
テキストファイルを読み込むには、次のようなKotlinコードを使用します。

```
val file = File("sample.txt")
file.forEachLine {println(it)} 
```

このコードは、`sample.txt`ファイルを開き、各行を読み取り、出力します。`forEachLine`関数は、各行を変数`it`に代入しています。また、ファイルを開く際には、ファイルの名前を指定する必要があります。

もしファイルを読み込む際に、特定の文字コードを使用したい場合は、次のように`charset`パラメーターを指定することができます。

```
val file = File("sample.txt")
file.forEachLine(charset = Charsets.UTF_8) {
    println(it)
}
```

この例では、UTF-8の文字コードを使用してファイルを読み込んでいます。

## ディープダイブ
テキストファイルを読み込む際には、ファイルのフォーマットによって異なる方法でデータを読み取る必要があります。例えば、CSVファイルのように、コンマで区切られたデータを読み取る場合は、正規表現や特定のライブラリを使用することができます。

また、例外処理も重要です。ファイルが存在しない場合やアクセス権限がない場合など、読み取りに失敗する可能性があります。そのため、try-catch文を使用して例外を処理する必要があります。

## 参考リンク
- [Kotlin 公式ドキュメント - ファイル処理](https://kotlinlang.org/docs/tutorials/kotlin-for-py/file-input-and-output.html)
- [Kotlin 入門チュートリアル: テキストファイルを読み取る](https://www.tutorialkart.com/kotlin/kotlin-read-a-text-file-examples/)
- [TextファイルをJava・Kotlinで読み込む方法](https://qiita.com/takuma-jpn/items/bb21721367db648fc499)
- [Kotlin: 例外処理の基本とtry-catch文の使い方](https://qiita.com/hajihaji1209/items/e601481b9f8be194e0f5)

## 関連リンク
- [Kotlin 公式ドキュメント](https://kotlinlang.org/docs/home.html)
- [Kotlin 言語リファレンス](https://kotlinlang.org/docs/reference/)