---
title:                "ロギング"
aliases:
- /ja/clojure/logging/
date:                  2024-01-26T01:01:16.337527-07:00
model:                 gpt-4-1106-preview
simple_title:         "ロギング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/logging.md"
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
