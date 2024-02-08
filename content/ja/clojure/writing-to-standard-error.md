---
title:                "標準エラーへの書き込み"
date:                  2024-02-03T19:33:00.322160-07:00
model:                 gpt-4-0125-preview
simple_title:         "標準エラーへの書き込み"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
