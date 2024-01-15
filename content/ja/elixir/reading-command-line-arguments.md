---
title:                "コンピュータープログラミングの記事のタイトル: 「コマンドライン引数の読み込み」"
html_title:           "Elixir: コンピュータープログラミングの記事のタイトル: 「コマンドライン引数の読み込み」"
simple_title:         "コンピュータープログラミングの記事のタイトル: 「コマンドライン引数の読み込み」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読むことのメリットは何でしょうか？それについて少し紹介します。

## 使い方

以前にも触れたように、コマンドライン引数を読むことで、プログラムを実行する際にユーザーが入力した特定の値を取得することができます。それでは、具体的なコード例を見てみましょう。

```Elixir
defmodule CLI do
  # 引数を受け取るためにOptionParserを使う
  def parse_args() do
    OptionParser.parse(System.argv) 
  end

  # 引数が存在しない場合はエラーを表示する
  def handle_args({:ok, args}, default) do
    # 引数が1つ以上ある場合、最初の引数を返す
    [arg | _] = args
    arg
  end
  
  # 引数が存在しない場合はデフォルト値を返す
  def handle_args({:error, _}, default) do
    IO.puts "Please provide an argument."
    default
  end
end

# コマンドライン引数を取得する
arg = CLI.parse_args() |> CLI.handle_args("default")

# コマンドライン引数を表示する
IO.puts "Your argument is: #{arg}"
```

この例では、 `parse_args` 関数を定義して、 `OptionParser` モジュールを使用して `System.argv` をパースしています。引数が存在する場合、 `handle_args` 関数に移動して、最初の引数を返します。もし引数が存在しない場合は、デフォルト値を返します。これで、コマンドライン引数を読む方法がわかりました。

## 詳細

コマンドライン引数を使用する際には、エラーハンドリングやデフォルト値の設定など、さまざまな状況に対する考慮が必要です。また、引数を複数取得したい場合は、リスト形式で取得することも可能です。さらに詳しい情報は、Elixirの公式ドキュメントを参考にしてください。

## 関連リンク

- [Elixir公式ドキュメント](https://elixir-lang.org/docs.html)
- [Elixirのコマンドライン引数](https://elixir-lang.org/getting-started/mix-otp/command-line.html#parsing-command-line-options-with-optionparser)