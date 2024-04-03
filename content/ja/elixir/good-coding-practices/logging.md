---
date: 2024-01-26 01:03:05.725338-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.620472-06:00'
model: gpt-4-1106-preview
summary: "\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u958B\u767A\u3067\u306E\u30ED\u30AE\
  \u30F3\u30B0\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u5B9F\u884C\u3055\
  \u308C\u3066\u3044\u308B\u9593\u306B\u767A\u751F\u3059\u308B\u30A4\u30D9\u30F3\u30C8\
  \u3092\u8A18\u9332\u3059\u308B\u6280\u8853\u3067\u3001\u901A\u5E38\u306F\u30D5\u30A1\
  \u30A4\u30EB\u3084\u5916\u90E8\u30B7\u30B9\u30C6\u30E0\u306B\u8A18\u9332\u3055\u308C\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\
  \u3044\u3001\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u306E\u6319\u52D5\u3092\u7406\u89E3\
  \u3057\u3001\u554F\u984C\u3092\u30C8\u30E9\u30D6\u30EB\u30B7\u30E5\u30FC\u30C6\u30A3\
  \u30F3\u30B0\u3057\u3001\u52D5\u4F5C\u5C65\u6B74\u3092\u8A18\u9332\u3059\u308B\u305F\
  \u3081\u3067\u3059\u3002\u3053\u308C\u306F\u30C7\u30D0\u30C3\u30B0\u3084\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u5065\u5EB7\u3092\u76E3\u8996\u3059\u308B\
  \u4E0A\u3067\u91CD\u8981\u3067\u3059\u3002."
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

## どのようにして：
Elixirで情報をログする主な方法は、組み込みの `Logger` モジュールを通してです。以下はその使用方法です：

```elixir
defmodule MyApplication do
  require Logger
  
  def do_something_important(param) do
    Logger.info("重要な処理をパラメーターで開始: #{param}")

    # 作業が行われていることをシミュレート
    :timer.sleep(1000)

    Logger.debug("処理が完了しました。")
  rescue
    error -> Logger.error("エラーが発生しました: #{inspect(error)}")
  end
end

# ログを確認するには、関数を呼び出すだけです：
MyApplication.do_something_important("MyParam")
```

このシンプルなコードスニペットは、異なるレベル（`info`、`debug`、`error`）でログする方法を示しています。これを実行すると、`Logger` のレベルを `:debug` に設定しなければ、debugメッセージは表示されません。デフォルトでは、ElixirのLoggerは `:info` より低いログメッセージをフィルタリングします。

`:info` レベルでのサンプル出力は次のようになります：
```
14:32:40.123 [info]  重要な処理をパラメーターで開始: MyParam
14:32:41.126 [error] エラーが発生しました: %RuntimeError{message: "runtime error"}
```

## ディープダイブ：
Elixirの `Logger` は初期のころから言語に組み込まれているツールで、Erlangのような他のBEAM言語のロギングシステムの影響を受けています。Loggerは異なるレベルのロギング – `:debug`、`:info`、`:warn`、`:error` – を提供し、ログメッセージを処理するための異なるバックエンドを接続可能にしています。

より複雑なシナリオに対して組み込みのLoggerの代替として、`Logstash` や Elixirの `Sentry` などのロギングライブラリを使用することもできます。これらは、より視覚的な形式でエラー追跡や集約のような追加機能を提供することができます。ローカル開発において、Elixir開発者はそのシンプルさとBEAM VMとの統合のために、しばしば組み込みのLogger機能に頼ります。

内部では、Loggerモジュールは非同期と同期のロギングを提供します。デフォルトの非同期ロギングは、メッセージをログに記録している間アプリケーションの実行をブロックしません。これは、ロギングがパフォーマンスに悪影響を及ぼさないことを保証します。しかし、送信された順序でメッセージがログされることを保証する必要がある場合には、同期ロギングを有効にすることができます。

Loggerの設定は、Elixirアプリケーションの `config/config.exs` ファイルで調整することができ、ログレベル、形式、メタデータなどを設定できます。常に、異なる環境でのログレベルと出力を調整してください。本番システムで冗長なデバッグログが溢れることを望まないでしょう。

## 参照：
- 公式のElixir Loggerドキュメント：https://hexdocs.pm/logger/Logger.html
- Elixirのロギングベストプラクティスに関するブログ投稿：https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Hex上のElixir用Sentry：https://hex.pm/packages/sentry
- LoggerについてのElixir Schoolのレッスン：https://elixirschool.com/en/lessons/specifics/debugging/#logging
