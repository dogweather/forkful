---
title:                "コマンドライン引数の読み取り"
date:                  2024-01-20T17:56:17.294367-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数を読み込むって何？プログラムが起動する時、ユーザーが指定した追加の情報です。なぜ使うの？プログラムの振る舞いを動的に変えたい時、重要な手段だからです。

## How to (方法)
Kotlinでコマンドライン引数を扱うのは簡単です。`main` 関数のパラメータとして `args: Array<String>` を使います。

```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("引数が見つかりました:")
        for (argument in args) {
            println(argument)
        }
    } else {
        println("引数はありません。")
    }
}
```

実行例:

```
> kotlinc Main.kt -include-runtime -d Main.jar
> java -jar Main.jar こんにちは 世界
引数が見つかりました:
こんにちは
世界
```

## Deep Dive (詳細な解説)
コマンドライン引数は歴史的にプログラムへの初期のインターフェース方法でした。Kotlinでは、他の言語（例：Java）と同様に扱います。`main`関数内で`args`配列にアクセスし、`Array<String>`型の引数を扱います。Kotlin1.3以前では、`Array<String>`を明示的に指定する必要がありましたが、1.3以降はこのパラメータを省略可能になりました。

例として、Apache Commons CLIやKotlinx CLIなどの外部ライブラリを使って、より複雑なコマンドライン処理を行うこともできます。実装詳細に興味があるなら、ソースコードを読むのも一つの手です。

## See Also (参照)
- Kotlin公式ドキュメンテーション: https://kotlinlang.org/docs/command-line.html
- Kotlinx CLI: https://github.com/Kotlin/kotlinx-cli
- Apache Commons CLI: https://commons.apache.org/proper/commons-cli/
