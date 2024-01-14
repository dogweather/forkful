---
title:                "Elixir: テキストファイルを読み込む"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

＃＃ なぜ

テキストファイルを読むのに読み手にメリットがありますか？テキストは、エディターを通じてコードやデータを変更するのに非常に一般的な方法です。ファイルを読むことで、エディターで編集されたデータを入手できるだけでなく、コードやデータをバックアップすることもできます。

＃＃ 方法

Elixirでは、ファイルを読むためにFileモジュールを使用することができます。以下のコードは、readme.txtという名前のファイルを読み込んで、ファイル内のテキストをすべて表示する例です。

```Elixir
ファイル=ファイル.オープン！（「readme.txt」）
contents = File.read！（file）
IO.putputs（contents）
```

上記のコードを実行すると、readme.txtファイルの内容がターミナルに表示されます。

＃＃ ディープダイブ

ファイルを読むには、`File.read！`と`File.readline！`の2つの関数があります。`File.read！`はファイルのすべての内容を文字列として返し、`File.readline！`は1行ずつ読み込みます。また、ファイルを開く際にオプションを指定することもできます。例えば、`:write`オプションを指定すると、ファイルを書き込み可能なモードで開くことができます。詳細な情報については、Elixirの公式ドキュメントを参照してください。

＃＃参照

＃＃＃ わからないことがある場合や、詳しい情報を知りたい場合は、以下のリンクを参考にしてください。

- Elixirの公式ドキュメント（https://elixir-lang.org/docs.html）
- Fileモジュールのドキュメント（https://hexdocs.pm/elixir/File.html）
- テキストファイルの読み書きについてのElixirフォーラムのディスカッション（https://elixirforum.com/t/reading-and-writing-to-a-text-file/2989）