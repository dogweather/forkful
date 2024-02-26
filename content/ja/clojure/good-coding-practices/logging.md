---
date: 2024-01-26 01:01:16.337527-07:00
description: "\u30ED\u30B0\u3068\u306F\u3001\u57FA\u672C\u7684\u306B\u306F\u8239\u306E\
  \u822A\u6D77\u65E5\u8A8C\u306B\u76F8\u5F53\u3059\u308B\u30BD\u30D5\u30C8\u30A6\u30A7\
  \u30A2\u30D0\u30FC\u30B8\u30E7\u30F3\u3067\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u304C\u5B9F\u884C\u4E2D\u306B\u767A\u751F\u3059\u308B\u30A4\u30D9\u30F3\
  \u30C8\u3092\u8A18\u9332\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u306B\u3088\u3063\
  \u3066\u3001\u30C7\u30D0\u30C3\u30B0\u3001\u76E3\u67FB\u306E\u305F\u3081\u306E\u8A18\
  \u9332\u3001\u307E\u305F\u306F\u672C\u756A\u30B7\u30B9\u30C6\u30E0\u306E\u6319\u52D5\
  \u3092\u6D1E\u5BDF\u3059\u308B\u305F\u3081\u306B\u30A4\u30D9\u30F3\u30C8\u3092\u8FFD\
  \u8DE1\u3057\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:39.719120-07:00'
model: gpt-4-1106-preview
summary: "\u30ED\u30B0\u3068\u306F\u3001\u57FA\u672C\u7684\u306B\u306F\u8239\u306E\
  \u822A\u6D77\u65E5\u8A8C\u306B\u76F8\u5F53\u3059\u308B\u30BD\u30D5\u30C8\u30A6\u30A7\
  \u30A2\u30D0\u30FC\u30B8\u30E7\u30F3\u3067\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u304C\u5B9F\u884C\u4E2D\u306B\u767A\u751F\u3059\u308B\u30A4\u30D9\u30F3\
  \u30C8\u3092\u8A18\u9332\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u306B\u3088\u3063\
  \u3066\u3001\u30C7\u30D0\u30C3\u30B0\u3001\u76E3\u67FB\u306E\u305F\u3081\u306E\u8A18\
  \u9332\u3001\u307E\u305F\u306F\u672C\u756A\u30B7\u30B9\u30C6\u30E0\u306E\u6319\u52D5\
  \u3092\u6D1E\u5BDF\u3059\u308B\u305F\u3081\u306B\u30A4\u30D9\u30F3\u30C8\u3092\u8FFD\
  \u8DE1\u3057\u307E\u3059\u3002"
title: "\u30ED\u30AE\u30F3\u30B0"
---

{{< edit_this_page >}}

## 何となぜ？
ログとは、基本的には船の航海日誌に相当するソフトウェアバージョンで、アプリケーションが実行中に発生するイベントを記録する方法です。プログラマーはこれを行うことによって、デバッグ、監査のための記録、または本番システムの挙動を洞察するためにイベントを追跡します。

## 方法：
ClojureはJavaのロギング機構に依存していますが、よりClojureらしい方法でそれらを活用することができます。`clojure.tools.logging`の使用方法を見てみましょう。これは複数のロギングフレームワークに対するシンプルな抽象化を提供しています：

まず、`project.clj`に`clojure.tools.logging`とロギング実装（例えば`log4j`）の依存関係を追加します：

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

次に、メッセージをログに記録してみましょう：

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "Starting intense computation...")
  (Thread/sleep 3000) ; 長い計算を模擬
  (log/info "Computation done. The answer is 42.")
  42)

(compute-answer-to-everything)
```
デフォルトでは`DEBUG`メッセージは表示されません。通常、ログレベルは`INFO`に設定されています：

```
INFO  [your-namespace] - Computation done. The answer is 42.
```

必要に応じて、`log4j.properties`ファイルでログレベルとアペンダーを設定して、より詳細な出力を得ることができます。

## 詳細な潜入
Clojureの`clojure.tools.logging`はしばらく前からあり、ClojureコードとJavaロギングの世界の橋渡しをします。歴史的に、JavaはビルトインのログAPI、`log4j`、`slf4j`、`logback`など、いくつかのロギングライブラリを経てきました。

Clojureでは、Javaのロギングフレームワークを直接使用することもできますが、`clojure.tools.logging`はクラスパスで見つかったロギングフレームワークに委譲することで、特定の実装に密接に結び付くことを避けられます。これにより、より移植性とモジュール性の高いClojureコードを保つ手助けになります。

Clojureエコシステム内の`clojure.tools.logging`への代替としては、ログローテーション、フィルタリング、箱出しの非同期ロギング機能などを備えた純粋なClojureロギングライブラリである`timbre`などのライブラリがあります。

ログは副作用として、特にClojureのようなマルチスレッド環境では慎重に扱われるべきです。不変性と副作用管理は、パフォーマンスのボトルネックを避け、スレッドセーフを保証するために明確な利点を提供します。これは、ほとんどのJavaロギングフレームワークが既にケアしていることです。

最後に、構造化ログについて考えてみましょう。ログを構造化データ（JSONなど）として書くことで、特に大規模な分散システムを扱う際に後での分析や処理に非常に役立ちます。

## 参照資料
もっと学びたい方は、以下のリソースをチェックしてみてください：

- Clojure Tools Loggingドキュメンテーション：https://github.com/clojure/tools.logging
- Timbre、Clojureのロギングライブラリ：https://github.com/ptaoussanis/timbre
- ClojureでLog4Jを設定する：http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Logbackマニュアル（高度な設定用）：http://logback.qos.ch/manual/
- Clojureにおける構造化ログに関するガイド：https://corfield.org/blog/2020/04/28/structured-logging/
