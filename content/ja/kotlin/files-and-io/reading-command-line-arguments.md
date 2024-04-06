---
date: 2024-01-20 17:56:17.294367-07:00
description: "How to (\u65B9\u6CD5) Kotlin\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\
  \u30F3\u5F15\u6570\u3092\u6271\u3046\u306E\u306F\u7C21\u5358\u3067\u3059\u3002`main`\
  \ \u95A2\u6570\u306E\u30D1\u30E9\u30E1\u30FC\u30BF\u3068\u3057\u3066 `args: Array<String>`\
  \ \u3092\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.966336-06:00'
model: gpt-4-1106-preview
summary: "Array<String>` \u3092\u4F7F\u3044\u307E\u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
