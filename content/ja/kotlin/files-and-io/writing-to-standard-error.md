---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:55.732587-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A Kotlin\u3067stderr\u3078\u306E\u66F8\
  \u304D\u8FBC\u307F\u306F\u3001`System.err.println()`\u3092\u4F7F\u3063\u3066\u9054\
  \u6210\u3067\u304D\u307E\u3059\u3002\u3053\u306E\u30E1\u30BD\u30C3\u30C9\u306F`System.out.println()`\u3068\
  \u4F3C\u3066\u3044\u307E\u3059\u304C\u3001\u6A19\u6E96\u51FA\u529B\u30B9\u30C8\u30EA\
  \u30FC\u30E0\u3067\u306F\u306A\u304F\u6A19\u6E96\u30A8\u30E9\u30FC\u30B9\u30C8\u30EA\
  \u30FC\u30E0\u3078\u51FA\u529B\u3092\u5411\u3051\u307E\u3059\u3002"
lastmod: '2024-04-05T22:38:41.631971-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u3046\u3084\u3063\u3066\uFF1A Kotlin\u3067stderr\u3078\u306E\u66F8\
  \u304D\u8FBC\u307F\u306F\u3001`System.err.println()`\u3092\u4F7F\u3063\u3066\u9054\
  \u6210\u3067\u304D\u307E\u3059\u3002\u3053\u306E\u30E1\u30BD\u30C3\u30C9\u306F`System.out.println()`\u3068\
  \u4F3C\u3066\u3044\u307E\u3059\u304C\u3001\u6A19\u6E96\u51FA\u529B\u30B9\u30C8\u30EA\
  \u30FC\u30E0\u3067\u306F\u306A\u304F\u6A19\u6E96\u30A8\u30E9\u30FC\u30B9\u30C8\u30EA\
  \u30FC\u30E0\u3078\u51FA\u529B\u3092\u5411\u3051\u307E\u3059\u3002"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

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
