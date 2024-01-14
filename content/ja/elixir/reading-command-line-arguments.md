---
title:    "Elixir: コンピュータプログラミング：コマンドライン引数の読み取り"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## なぜ読むのか
コマンドライン引数を読む方法について学ぶことで、Elixirプログラマーにとってより使いやすいアプリケーションを作ることができます。

## 読み方
コマンドライン引数を読むには、`OptionParser`モジュールを使用することができます。例として、以下のように引数を受け取り、出力するプログラムを作成します。

```Elixir
defmodule CommandLine do
  def main do
    {opts, args, params} = OptionParser.parse(ARGV, strict: [foo: :string, bar: :integer, baz: :boolean])
    IO.inspect(opts)
    IO.inspect(args)
    IO.inspect(params)
  end
end

CommandLine.main()
```
実行すると、以下のように引数がパースされ、出力されます。

```Elixir
foo: "hello"
bar: 123
baz: true

["some", "extra", "arguments"]

["--flag", "value1", "--flag", "value2"]
```

## 深い調査
`OptionParser`モジュールでは、厳密な引数の指定や、複数の値を受け取ることなど、さまざまなオプションを設定することができます。詳細については、公式ドキュメントを参照してください。

## 参考リンク
- `OptionParser`モジュールの公式ドキュメント [https://hexdocs.pm/elixir/OptionParser.html](https://hexdocs.pm/elixir/OptionParser.html)
- 引数の解析・パースについてのさらなる調査 [https://medium.com/elixirlabs/the-elixir-way-a-thorough-guide-to-elixirs-optionsparser-a409b69045d](https://medium.com/elixirlabs/the-elixir-way-a-thorough-guide-to-elixirs-optionsparser-a409b69045d)

---
参考までに、この記事はMarkdown形式で書かれており、プレーンテキストとしても読むことができます。