---
date: 2024-01-26 01:01:16.337527-07:00
description: "\u65B9\u6CD5\uFF1A Clojure\u306FJava\u306E\u30ED\u30AE\u30F3\u30B0\u6A5F\
  \u69CB\u306B\u4F9D\u5B58\u3057\u3066\u3044\u307E\u3059\u304C\u3001\u3088\u308AClojure\u3089\
  \u3057\u3044\u65B9\u6CD5\u3067\u305D\u308C\u3089\u3092\u6D3B\u7528\u3059\u308B\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002`clojure.tools.logging`\u306E\u4F7F\u7528\
  \u65B9\u6CD5\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002\u3053\u308C\u306F\
  \u8907\u6570\u306E\u30ED\u30AE\u30F3\u30B0\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\
  \u306B\u5BFE\u3059\u308B\u30B7\u30F3\u30D7\u30EB\u306A\u62BD\u8C61\u5316\u3092\u63D0\
  \u4F9B\u3057\u3066\u3044\u307E\u3059\uFF1A\u2026"
lastmod: '2024-03-13T22:44:41.567735-06:00'
model: gpt-4-1106-preview
summary: "Clojure\u306FJava\u306E\u30ED\u30AE\u30F3\u30B0\u6A5F\u69CB\u306B\u4F9D\u5B58\
  \u3057\u3066\u3044\u307E\u3059\u304C\u3001\u3088\u308AClojure\u3089\u3057\u3044\u65B9\
  \u6CD5\u3067\u305D\u308C\u3089\u3092\u6D3B\u7528\u3059\u308B\u3053\u3068\u304C\u3067\
  \u304D\u307E\u3059\u3002`clojure.tools.logging`\u306E\u4F7F\u7528\u65B9\u6CD5\u3092\
  \u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002\u3053\u308C\u306F\u8907\u6570\u306E\
  \u30ED\u30AE\u30F3\u30B0\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u306B\u5BFE\u3059\
  \u308B\u30B7\u30F3\u30D7\u30EB\u306A\u62BD\u8C61\u5316\u3092\u63D0\u4F9B\u3057\u3066\
  \u3044\u307E\u3059\uFF1A\n\n\u307E\u305A\u3001`project.clj`\u306B`clojure.tools.logging`\u3068\
  \u30ED\u30AE\u30F3\u30B0\u5B9F\u88C5\uFF08\u4F8B\u3048\u3070`log4j`\uFF09\u306E\u4F9D\
  \u5B58\u95A2\u4FC2\u3092\u8FFD\u52A0\u3057\u307E\u3059\uFF1A."
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

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
