---
date: 2024-01-26 01:03:05.725338-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Elixir\u3067\u60C5\u5831\
  \u3092\u30ED\u30B0\u3059\u308B\u4E3B\u306A\u65B9\u6CD5\u306F\u3001\u7D44\u307F\u8FBC\
  \u307F\u306E `Logger` \u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u901A\u3057\u3066\u3067\
  \u3059\u3002\u4EE5\u4E0B\u306F\u305D\u306E\u4F7F\u7528\u65B9\u6CD5\u3067\u3059\uFF1A\
  ."
lastmod: '2024-04-05T22:37:49.951221-06:00'
model: gpt-4-1106-preview
summary: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Elixir\u3067\u60C5\u5831\
  \u3092\u30ED\u30B0\u3059\u308B\u4E3B\u306A\u65B9\u6CD5\u306F\u3001\u7D44\u307F\u8FBC\
  \u307F\u306E `Logger` \u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u901A\u3057\u3066\u3067\
  \u3059\u3002\u4EE5\u4E0B\u306F\u305D\u306E\u4F7F\u7528\u65B9\u6CD5\u3067\u3059\uFF1A\
  ."
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
