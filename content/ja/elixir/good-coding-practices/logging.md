---
title:                "ロギング"
aliases:
- /ja/elixir/logging/
date:                  2024-01-26T01:03:05.725338-07:00
model:                 gpt-4-1106-preview
simple_title:         "ロギング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/logging.md"
---

{{< edit_this_page >}}

## 何となぜか？
ソフトウェア開発でのロギングとは、プログラムが実行されている間に発生するイベントを記録する技術で、通常はファイルや外部システムに記録されます。プログラマーはこれを行い、ソフトウェアの挙動を理解し、問題をトラブルシューティングし、動作履歴を記録するためです。これはデバッグやアプリケーションの健康を監視する上で重要です。

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
