---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何とその理由は何ですか？

コマンドライン引数を読み取るとは、プログラムが開始されるときにユーザーから提供されたデータを扱う方法です。これにより、プログラマーはプログラムが稼働している間に変化する可能性のある設定を指定したり、ユーザーの特定の要求に対応したりできます。

## 方法：

下に、Kotlinでのコマンドライン引数の読み取り方を示します。

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
      println(arg)
    }
}
```

このプログラムを `Hello Kotlin` という引数で実行すると、出力は次のようになります：

```
Hello
Kotlin
```

## 深堀り：

コマンドライン引数の使用は、古くから存在するプログラミング概念です。初期のコマンドライン環境では、個々のプログラムがシステムと相互作用する基本的な方法でした。Kotlinでは、`main`関数の`args`パラメータを通じて引数を取得します。

読み取ることができる別の方法として、Javaの`Scanner`クラスを使用する方法や、ライブラリ（例えば`kotlinx.cli`）を使用する方法があります。  

実装に関する具体的な詳細は、Kotlinの実行環境（たとえば、JVM上で実行するか、ネイティブで実行するか）によってわずかに異なる可能性があります。

## 参考資料：

JVMのコマンドライン引数の詳細については[こちら](https://www.baeldung.com/java-command-line-arguments)をご覧ください。  

KotlinのコマンドラインライブラリについてはKotlinの公式ドキュメンテーション[こちら](https://github.com/Kotlin/kotlinx.cli)をご覧ください。