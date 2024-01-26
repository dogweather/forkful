---
title:                "ロギング"
date:                  2024-01-26T01:07:26.374442-07:00
model:                 gpt-4-1106-preview
simple_title:         "ロギング"
programming_language: "Java"
category:             "Java"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/logging.md"
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