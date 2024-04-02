---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:00.322160-07:00
description: "\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\u66F8\u304D\
  \u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\u8A3A\
  \u65AD\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\u3068\u306F\u5225\u306Estderr\u30B9\
  \u30C8\u30EA\u30FC\u30E0\u306B\u5411\u3051\u3066\u51FA\u529B\u3059\u308B\u3053\u3068\
  \u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\
  \u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u901A\u5E38\u306E\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u51FA\u529B\u3068\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\
  \u533A\u5225\u3057\u3001\u3088\u308A\u52B9\u679C\u7684\u306A\u30C7\u30D0\u30C3\u30B0\
  \u3068\u30ED\u30B0\u8A18\u9332\u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.580532-06:00'
model: gpt-4-0125-preview
summary: "\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\u66F8\u304D\
  \u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3084\u8A3A\
  \u65AD\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\u3068\u306F\u5225\u306Estderr\u30B9\
  \u30C8\u30EA\u30FC\u30E0\u306B\u5411\u3051\u3066\u51FA\u529B\u3059\u308B\u3053\u3068\
  \u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\
  \u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u901A\u5E38\u306E\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u51FA\u529B\u3068\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\
  \u533A\u5225\u3057\u3001\u3088\u308A\u52B9\u679C\u7684\u306A\u30C7\u30D0\u30C3\u30B0\
  \u3068\u30ED\u30B0\u8A18\u9332\u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\u3059\u3002"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 何となぜ？
標準エラー（stderr）への書き込みは、エラーメッセージや診断を標準出力（stdout）とは別のstderrストリームに向けて出力することについてです。プログラマはこれを行うことで、通常のプログラム出力とエラーメッセージを区別し、より効果的なデバッグとログ記録が可能になります。

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
