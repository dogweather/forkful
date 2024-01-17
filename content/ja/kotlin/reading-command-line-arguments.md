---
title:                "コンピューター・プログラミングに関する記事タイトル：コマンドライン引数の読み取り"
html_title:           "Kotlin: コンピューター・プログラミングに関する記事タイトル：コマンドライン引数の読み取り"
simple_title:         "コンピューター・プログラミングに関する記事タイトル：コマンドライン引数の読み取り"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## それは何で、なんで必要なの？
コマンドライン引数を読み取るとは、プログラマーがプログラムを実行する際に指定したパラメーターを取得することです。これにより、プログラムが実行する内容をユーザーが制御することができます。

## 方法：
```Kotlin 
fun main(args: Array<String>){
    println("あなたの名前は ${args[0]} です")
}
```
上記の例では、コマンドラインから渡された最初の引数を使用して、ユーザーの名前を表示しています。もちろん、他の引数を使用することもできます。

## 深く掘り下げる：
コマンドライン引数を読み取る機能は、プログラミング言語によって異なりますが、ほとんどの言語ではネイティブなサポートを提供しています。また、コマンドライン引数を使用する代替手段として、環境変数やコンフィギュレーションファイルを使用することもできます。コマンドライン引数の実装には、コマンドラインの解析やエラーハンドリングなどが含まれます。

## 関連リンク:
- [Kotlin公式ドキュメント](https://kotlinlang.org/docs/reference/command-line.html)
- [コマンドライン引数についての記事](https://medium.com/@scottshipp/reading-command-line-arguments-in-kotlin-964a6740ddd9)