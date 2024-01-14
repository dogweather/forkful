---
title:                "Kotlin: コンピュータープログラミングにおける「コマンドライン引数の読み取り」"
simple_title:         "コンピュータープログラミングにおける「コマンドライン引数の読み取り」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##なぜ

コマンドライン引数を読むことについて学びたい方のために、この記事を書きました！コマンドライン引数を読むことによって、プログラムをより動的に制御することができます。

##やり方

まず、コマンドライン引数を読むためには、 `main`関数の引数に`args`という名前の配列を追加します。

```Kotlin 
fun main(args: Array<String>) {
    // コマンドライン引数の数を入力します
    println(args.size) 
}
```

コマンドラインでプログラムを実行するとき、引数をスペースで区切って入力します。例えば、`kotlin program.kt arg1 arg2`のように入力すると、`args`配列には`"arg1"`と`"arg2"`という値が含まれます。

もう少し複雑な例を見てみましょう。以下のようなプログラムを実行した場合、

```Kotlin 
fun main(args: Array<String>) {
  // 第1引数を整数に変換して`num`変数に代入します
  var num = args[0].toInt() 
  println(num + 5) // 結果は11になります
}
```

`kotlin program.kt 6`と入力すると、結果は11になります。コマンドライン引数は文字列として渡されるため、数値として使用するには`toInt()`関数を使用して変換する必要があります。

##ディープダイブ

コマンドライン引数を読むときには、いくつかの注意点があります。まず、引数を指定しない場合、`args`配列は空になります。そのため、この場合はエラーハンドリングを行う必要があります。

また、引数が多すぎる場合も同様のエラーが発生します。そのため、`args`配列のサイズをチェックしてから使用するようにしましょう。

さらに、コマンドライン引数にはオプションを指定することもできます。例えば、`-f`というオプションを指定して、ファイルを作成するプログラムを考えてみましょう。この場合、`args`配列には`-f`という文字列を特定の位置に指定する必要があります。そのため、`args`配列の中身を順番にチェックして、必要なオプションが指定されているかどうかを確認する必要があります。

##参考情報

- [Kotlin公式ドキュメントのコマンドライン引数の使用例](https://kotlinlang.org/docs/command-line.html#using-command-line-arguments)
- [Kotlinプログラミング入門ブログ](https://kotlinlang.org/blog/kotlin-programming-intro-blog.html)
- [JavaとKotlinを使い分ける](https://techblog.zozo.com/entry/use-java-class-with-kotlin-in-android-project)