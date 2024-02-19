---
aliases:
- /ja/java/logging/
date: 2024-01-26 01:07:26.374442-07:00
description: "\u30ED\u30B0\u3068\u306F\u3001\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u767A\u751F\u3059\u308B\u30A4\
  \u30D9\u30F3\u30C8\u3092\u8A18\u9332\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u306E\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30E9\u30F3\
  \u30BF\u30A4\u30E0\u60C5\u5831\u3092\u30AD\u30E3\u30D7\u30C1\u30E3\u3057\u305F\u308A\
  \u3001\u554F\u984C\u3092\u30C7\u30D0\u30C3\u30B0\u3057\u305F\u308A\u3001\u30B7\u30B9\
  \u30C6\u30E0\u306E\u6319\u52D5\u3092\u76E3\u8996\u3057\u305F\u308A\u3001\u30BB\u30AD\
  \u30E5\u30EA\u30C6\u30A3\u3084\u30B3\u30F3\u30D7\u30E9\u30A4\u30A2\u30F3\u30B9\u306E\
  \u76EE\u7684\u3067\u76E3\u67FB\u7528\u306E\u8A18\u9332\u3092\u4F5C\u6210\u3059\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3089\u306E\u30A4\u30D9\u30F3\u30C8\u3092\u30ED\u30B0\
  \u306B\u8A18\u9332\u3057\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.807730
model: gpt-4-1106-preview
summary: "\u30ED\u30B0\u3068\u306F\u3001\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u3067\u767A\u751F\u3059\u308B\u30A4\
  \u30D9\u30F3\u30C8\u3092\u8A18\u9332\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u306E\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30E9\u30F3\
  \u30BF\u30A4\u30E0\u60C5\u5831\u3092\u30AD\u30E3\u30D7\u30C1\u30E3\u3057\u305F\u308A\
  \u3001\u554F\u984C\u3092\u30C7\u30D0\u30C3\u30B0\u3057\u305F\u308A\u3001\u30B7\u30B9\
  \u30C6\u30E0\u306E\u6319\u52D5\u3092\u76E3\u8996\u3057\u305F\u308A\u3001\u30BB\u30AD\
  \u30E5\u30EA\u30C6\u30A3\u3084\u30B3\u30F3\u30D7\u30E9\u30A4\u30A2\u30F3\u30B9\u306E\
  \u76EE\u7684\u3067\u76E3\u67FB\u7528\u306E\u8A18\u9332\u3092\u4F5C\u6210\u3059\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3089\u306E\u30A4\u30D9\u30F3\u30C8\u3092\u30ED\u30B0\
  \u306B\u8A18\u9332\u3057\u307E\u3059\u3002"
title: "\u30ED\u30AE\u30F3\u30B0"
---

{{< edit_this_page >}}

## 何となぜ？
ログとは、ソフトウェアアプリケーション内で発生するイベントを記録するプロセスのことです。プログラマーは、ランタイム情報をキャプチャしたり、問題をデバッグしたり、システムの挙動を監視したり、セキュリティやコンプライアンスの目的で監査用の記録を作成するためにこれらのイベントをログに記録します。

## どのように：
Javaでのログ取りを始める簡単な方法は、ビルトインの`java.util.logging`パッケージを使用することです。

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("INFOレベルのメッセージをログに記録");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "例外が発生", e);
        }
    }
}
```

これにより、以下のような出力が生成されます：

```
2023年7月3日 午後2時00分00秒 AppLogging main
情報: INFOレベルのメッセージをログに記録
2023年7月3日 午後2時00分00秒 AppLogging main
重大: 例外が発生
java.lang.ArithmeticException: / by zero
    at AppLogging.main(AppLogging.java:10)
```

## 深掘り
Javaにおけるログ取りはかなり進化しています。かつてのログはよりアドホックで、システム出力や自作のメカニズムで行われていました。しかしながら、標準化の必要性からログAPI、例えば`Log4j`や`SLF4J`などが生まれました。`java.util.logging`パッケージ自体はJDK 1.4で導入され、メッセージをログに記録するための標準化された方法を提供しています。

`java.util.logging`（JUL）の代替となるものにはLog4j 2やSLF4Jがあります。JULはJavaにビルトインされているため追加の依存関係は必要ありませんが、Log4j 2やSLF4Jは、ログ設定のより細かな制御、非同期ログ取り、より良いパフォーマンスなど、より進んだ機能を提供しています。

実装の面では、ログは同期的、つまり各ログメッセージがそれを生成したスレッドで処理されるか、非同期的、つまりメッセージが別のスレッドに渡されるかのどちらかになります。非同期ログ取りはパフォーマンスを向上させることができますが、同時にアプリケーションがクラッシュした際にログメッセージが失われないように並行処理を扱い、複雑さが増します。

## 参照
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Oracleの公式ログ概要](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [java.util.loggingに関するチュートリアル](https://www.vogella.com/tutorials/Logging/article.html)
