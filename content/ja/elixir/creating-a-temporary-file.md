---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ
一時ファイルを作成する理由は、一時的にデータを保存したり、一時的なデータ処理を行ったりするためです。

## 作り方
一時ファイルを作成するには、Elixirの標準ライブラリであるFileモジュールを使用します。まず、一時ファイルの保存先となるディレクトリを指定する必要があります。次に、File.tempfile/2関数を使用して、一時ファイルを作成します。以下のコードは、一時ファイルを作成し、その内容を書き込む例です。

```Elixir
File.tempfile("/tmp", "temp-") do |file|
  IO.write(file, "This is a temporary file.")
end

# 出力:
# {:ok, #File<"/tmp/temp-123456.txt">}
```

## 深掘り
一時ファイルを作成する際には、注意すべき点がいくつかあります。まず、一時ファイルはプログラムが終了すると自動的に削除されるため、データの永続的な保存には向きません。また、同名の一時ファイルがすでに存在する場合は、新しい一時ファイルが作成されず、既存のファイルが返される点にも注意してください。さらに、一時ファイルを作成する際は、標準ライブラリのFileモジュールの他にも、Elixirで標準的に使用されるExUnitやGenServerなどのライブラリが一時ファイルを作成するために使用されることがあります。

## 参考リンク
- [ElixirのFileモジュールのドキュメント](https://hexdocs.pm/elixir/File.html#tempfile/2)
- [ExUnitによる一時ファイルの作成の例](https://hexdocs.pm/ex_unit/1.11.0/ExUnit.Case.html#using-setup/2)
- [GenServerによる一時ファイルの作成の例](https://hexdocs.pm/elixir/GenServer.html#handle_continue/2)

## 参考になる情報
- [manページ: mktempコマンド](https://linuxjm.osdn.jp/html/GNU_coreutils/man1/mktemp.1.html)
- [manページ: tempfileコマンド](https://linuxjm.osdn.jp/html/GNU_coreutils/man1/tempfile.1.html)