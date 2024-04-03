---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:55.732587-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.088386-06:00'
model: gpt-4-0125-preview
summary: "\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\u66F8\u304D\
  \u8FBC\u307F\u3068\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\
  \u8A3A\u65AD\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\u3068\u306F\u5225\u306E\
  \u30B9\u30C8\u30EA\u30FC\u30E0\u306B\u51FA\u529B\u3059\u308B\u3053\u3068\u3092\u610F\
  \u5473\u3057\u307E\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u30A8\u30E9\u30FC\
  \u30CF\u30F3\u30C9\u30EA\u30F3\u30B0\u3068\u30ED\u30B0\u89E3\u6790\u304C\u5BB9\u6613\
  \u306B\u306A\u308A\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30C7\u30D0\u30C3\u30B0\u3092\u5BB9\u6613\u306B\u3057\u3001\u30A8\u30E9\u30FC\u30E1\
  \u30C3\u30BB\u30FC\u30B8\u3092\u7C21\u5358\u306B\u8B58\u5225\u3057\u3066\u5FC5\u8981\
  \u306B\u5FDC\u3058\u3066\u30EA\u30C0\u30A4\u30EC\u30AF\u30C8\u3067\u304D\u308B\u3088\
  \u3046\u306B\u3059\u308B\u305F\u3081\u306B\u3001\u3053\u306E\u65B9\u6CD5\u3092\u4F7F\
  \u3044\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u51FA\u529B\u30ED\u30B0\u3084\u30E6\
  \u30FC\u30B6\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u30AF\u30EA\u30FC\u30F3\u306B\
  \u4FDD\u3064\u3053\u3068\u306B\u5F79\u7ACB\u3061\u307E\u3059\u3002."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 何となぜ？

標準エラー（stderr）への書き込みとは、エラーメッセージや診断を標準出力（stdout）とは別のストリームに出力することを意味します。これにより、エラーハンドリングとログ解析が容易になります。プログラマーは、デバッグを容易にし、エラーメッセージを簡単に識別して必要に応じてリダイレクトできるようにするために、この方法を使います。これは、出力ログやユーザーメッセージをクリーンに保つことに役立ちます。

## どうやって：

Kotlinでstderrへの書き込みは、`System.err.println()`を使って達成できます。このメソッドは`System.out.println()`と似ていますが、標準出力ストリームではなく標準エラーストリームへ出力を向けます。

```kotlin
fun main() {
    System.err.println("これはエラーメッセージです！")
}
```

サンプル出力：
```
これはエラーメッセージです！
```

特にLogbackやSLF4Jのようなログフレームワークを使用する、より構造化されたまたは複雑なアプリケーションでは、特定のログレベル（例えば、ERROR）に対してstderrへログを書き込むようにロガーを設定できます。

SLF4JとLogbackを使用する：

1. まず、`build.gradle`にSLF4J APIとLogback実装を追加します：

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. 次に、Logbackを（`src/main/resources/logback.xml`で）エラーレベルのメッセージをstderrに向けるように設定します：

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. そして、KotlinのコードでSLF4Jを使ってエラーメッセージをログに記録します：

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("これはエラーログメッセージです！")
}
```

サンプル出力（stderrへ）：
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - これはエラーログメッセージです！
```
