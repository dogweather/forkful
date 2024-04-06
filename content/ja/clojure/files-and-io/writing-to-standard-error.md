---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:00.322160-07:00
description: "\u65B9\u6CD5\uFF1A Clojure\u3067\u306F\u3001`*err*`\u30B9\u30C8\u30EA\
  \u30FC\u30E0\u3092\u4F7F\u7528\u3057\u3066stderr\u306B\u66F8\u304D\u8FBC\u3080\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002\u57FA\u672C\u7684\u306A\u4F8B\u3092\u3053\
  \u3061\u3089\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:37:49.909720-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Clojure\u3067\u306F\u3001`*err*`\u30B9\u30C8\u30EA\u30FC\
  \u30E0\u3092\u4F7F\u7528\u3057\u3066stderr\u306B\u66F8\u304D\u8FBC\u3080\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002\u57FA\u672C\u7684\u306A\u4F8B\u3092\u3053\u3061\
  \u3089\u3067\u3059\uFF1A."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 方法：
Clojureでは、`*err*`ストリームを使用してstderrに書き込むことができます。基本的な例をこちらです：

```clojure
(.write *err* "これはエラーメッセージです。\n")
```

メッセージを書き込んだ後、ストリームをフラッシュして、メッセージが直ちに出力されることを確実にする必要があります：

```clojure
(flush)
```

stderrへのサンプル出力：
```
これはエラーメッセージです。
```

例外を処理している場合は、スタックトレースをstderrに出力したいかもしれません。これには `printStackTrace` を使用します：

```clojure
(try
  ;; 例外を投げる可能性のあるコード
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

より構造化されたエラーログには、`timbre` のようなサードパーティのライブラリをstderrへログを記録するように設定することができます。基本的なセットアップと使用方法は次の通りです：

まず、依存関係に `timbre` を追加します。次に、stderrを使用するように設定します：

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; stdoutのログ記録を無効にする
(timbre/set-config! [:appenders :spit :enabled?] false) ;; ファイルログ記録を無効にする
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; エラーについてはstderrを有効にする

(timbre/error "リクエストの処理中にエラーが発生しました。")
```

これにより、エラーレベルのメッセージがstderrに向けられ、標準アプリケーション出力と区別されます。
