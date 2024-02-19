---
aliases:
- /ja/kotlin/reading-command-line-arguments/
date: 2024-01-20 17:56:17.294367-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u307F\
  \u8FBC\u3080\u3063\u3066\u4F55\uFF1F\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u8D77\u52D5\
  \u3059\u308B\u6642\u3001\u30E6\u30FC\u30B6\u30FC\u304C\u6307\u5B9A\u3057\u305F\u8FFD\
  \u52A0\u306E\u60C5\u5831\u3067\u3059\u3002\u306A\u305C\u4F7F\u3046\u306E\uFF1F\u30D7\
  \u30ED\u30B0\u30E9\u30E0\u306E\u632F\u308B\u821E\u3044\u3092\u52D5\u7684\u306B\u5909\
  \u3048\u305F\u3044\u6642\u3001\u91CD\u8981\u306A\u624B\u6BB5\u3060\u304B\u3089\u3067\
  \u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.893718
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u307F\
  \u8FBC\u3080\u3063\u3066\u4F55\uFF1F\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u8D77\u52D5\
  \u3059\u308B\u6642\u3001\u30E6\u30FC\u30B6\u30FC\u304C\u6307\u5B9A\u3057\u305F\u8FFD\
  \u52A0\u306E\u60C5\u5831\u3067\u3059\u3002\u306A\u305C\u4F7F\u3046\u306E\uFF1F\u30D7\
  \u30ED\u30B0\u30E9\u30E0\u306E\u632F\u308B\u821E\u3044\u3092\u52D5\u7684\u306B\u5909\
  \u3048\u305F\u3044\u6642\u3001\u91CD\u8981\u306A\u624B\u6BB5\u3060\u304B\u3089\u3067\
  \u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
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
