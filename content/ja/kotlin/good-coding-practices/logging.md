---
aliases:
- /ja/kotlin/logging/
date: 2024-01-26 01:08:36.596699-07:00
description: "\u30ED\u30B0\u53D6\u308A\u306F\u3001\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u304B\u3089\u306E\u30A4\u30D9\u30F3\
  \u30C8\u3084\u30C7\u30FC\u30BF\u3092\u30D5\u30A1\u30A4\u30EB\u3084\u30B3\u30F3\u30BD\
  \u30FC\u30EB\u306A\u3069\u306E\u5916\u90E8\u51FA\u529B\u306B\u8A18\u9332\u3059\u308B\
  \u5B9F\u8DF5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30B3\u30FC\
  \u30C9\u3092\u8FFD\u8DE1\u3057\u305F\u308A\u3001\u554F\u984C\u3092\u30C8\u30E9\u30D6\
  \u30EB\u30B7\u30E5\u30FC\u30C6\u30A3\u30F3\u30B0\u3057\u305F\u308A\u3001\u30A2\u30D7\
  \u30EA\u306E\u6319\u52D5\u3092\u76E3\u8996\u3057\u3001\u4ED6\u306E\u65B9\u6CD5\u3067\
  \u306F\u52B9\u679C\u7684\u306B\u5F97\u3089\u308C\u306A\u3044\u91CD\u8981\u306A\u6D1E\
  \u5BDF\u3092\u63D0\u4F9B\u3059\u308B\u305F\u3081\u306B\u30ED\u30B0\u3092\u53D6\u308A\
  \u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.885010
model: gpt-4-1106-preview
summary: "\u30ED\u30B0\u53D6\u308A\u306F\u3001\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u304B\u3089\u306E\u30A4\u30D9\u30F3\
  \u30C8\u3084\u30C7\u30FC\u30BF\u3092\u30D5\u30A1\u30A4\u30EB\u3084\u30B3\u30F3\u30BD\
  \u30FC\u30EB\u306A\u3069\u306E\u5916\u90E8\u51FA\u529B\u306B\u8A18\u9332\u3059\u308B\
  \u5B9F\u8DF5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30B3\u30FC\
  \u30C9\u3092\u8FFD\u8DE1\u3057\u305F\u308A\u3001\u554F\u984C\u3092\u30C8\u30E9\u30D6\
  \u30EB\u30B7\u30E5\u30FC\u30C6\u30A3\u30F3\u30B0\u3057\u305F\u308A\u3001\u30A2\u30D7\
  \u30EA\u306E\u6319\u52D5\u3092\u76E3\u8996\u3057\u3001\u4ED6\u306E\u65B9\u6CD5\u3067\
  \u306F\u52B9\u679C\u7684\u306B\u5F97\u3089\u308C\u306A\u3044\u91CD\u8981\u306A\u6D1E\
  \u5BDF\u3092\u63D0\u4F9B\u3059\u308B\u305F\u3081\u306B\u30ED\u30B0\u3092\u53D6\u308A\
  \u307E\u3059\u3002"
title: "\u30ED\u30AE\u30F3\u30B0"
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
