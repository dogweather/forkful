---
title:                "ロギング"
aliases:
- /ja/kotlin/logging.md
date:                  2024-01-26T01:08:36.596699-07:00
model:                 gpt-4-1106-preview
simple_title:         "ロギング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/logging.md"
---

{{< edit_this_page >}}

## 何となぜ？

ログ取りは、ソフトウェアアプリケーションからのイベントやデータをファイルやコンソールなどの外部出力に記録する実践です。プログラマーはコードを追跡したり、問題をトラブルシューティングしたり、アプリの挙動を監視し、他の方法では効果的に得られない重要な洞察を提供するためにログを取ります。

## どうやって：

Kotlinでは、単純なケースでは組み込みの`println()`関数を使用してログ取りを行うことができます。またはSLF4J with LogbackやLog4jのようなより洗練されたライブラリで、高度な必要性に応じたログ取りが可能です。

以下は`println()`を使用した基本的な例です：

```Kotlin
fun main() {
    println("Simple log message: Application started.")
    // ... ここにアプリケーションのロジック ...
    try {
        // エラーをシミュレーション
        throw Exception("Simulated error")
    } catch (e: Exception) {
        println("Error log message: " + e.message)
    }
}
```

出力：
```
Simple log message: Application started.
Error log message: Simulated error
```

こちらはLogbackを用いて設定されたSLF4Jを使用するスニペットです：

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("Structured log message: App launched.")
    // ... ここにアプリケーションのロジック ...
    try {
        // エラーをシミュレーション
        throw Exception("Simulated error")
    } catch (e: Exception) {
        logger.error("Structured error log: ", e)
    }
}
```

適切なLogback構成を想定すると、出力は整形され、ログファイルに書き込まれる際に次のようになるかもしれません：
```
[INFO] - 2023-03-29 14:15:42 - MyAppLogger - Structured log message: App launched.
[ERROR] - 2023-03-29 14:15:43 - MyAppLogger - Structured error log: 
java.lang.Exception: Simulated error
   at com.myapp.Main.main(Main.kt:10)
```

## 深堀り

歴史的には、ログ取りはアプリケーションやシステムの複雑化と共に発展してきました。初期のプログラムでは単純なprintステートメントで十分でしたが、システムがネットワークされ、異なる環境で異なるユーザーによって実行されるようになると、堅牢で永続的なログシステムが不可欠になりました。

Kotlinが普及する前は、Java開発者はLog4jとその後SLF4Jなどのライブラリを広く採用していました。これらはKotlinでの類似の実践に影響を与えており、KotlinはJavaライブラリとの相互運用性を利用しています。SLF4Jは抽象化レイヤーとして機能し、実際のログ実装を交換可能にします—通常はLogbackやLog4j2が選択されます。

Kotlinは、`expect`/`actual`メカニズムを通じて、JVM、JavaScript、Nativeなど複数のプラットフォームにわたって機能する多機能ログソリューションも可能にします。これは、プラットフォーム固有の実装を抽象化します。

専用のログライブラリに対して、printlnは追加のセットアップや依存関係を必要としないため最も単純なログ形式として残っていますが、ログレベル、ログローテーション、構造化されたフォーマットなどの機能不足のため通常は本番アプリケーションには適していません。

高度なログフレームワークの他の一般的な機能には次のものがあります。

- ログメッセージの緊急性を分類するログレベル（DEBUG、INFO、WARN、ERRORなど）。
- コンソール、ファイル、データベース、ネットワークサービスなど様々なシンクへの出力。
- 自動的なログローテーションと保持ポリシー。
- マイクロサービスアーキテクチャのための分散トレーシングサポート。
- ログ分析システムとの統合を良好にするJSONなどの構造化ログを使用。

これらのツールと機能は、特に複雑で分散していたり、高度にスケールされた環境で、信頼性の高い監視可能なシステムを維持するためには不可欠です。

## さらに詳しく

Kotlinでのログ取りについてのさらなる学習と洞察のためには、以下をチェックしてください：

- SLF4J（Simple Logging Facade for Java） [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback、Log4jの後継 [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- 'expect' と 'actual' 宣言についての Kotlin Multiplatform ドキュメント：[https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- Kotlinで構造化ログ取りのガイド：[https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)
