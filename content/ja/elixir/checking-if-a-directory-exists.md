---
title:                "ディレクトリが存在するかどうかを確認する"
date:                  2024-01-19
html_title:           "Bash: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ディレクトリが存在するかを確認するとは、ファイルシステムに特定のフォルダが存在するかどうかをチェックすることです。プログラマーはファイル読み書き前やディレクトリ作成時にエラーを避けるためにこれを行います。

## How to: (やり方)
Elixirの`File`モジュールを使って、ディレクトリの存在を簡単に確認できます。

```elixir
# ディレクトリが存在するかチェック
if File.dir?("some_directory") do
  IO.puts("存在します！")
else
  IO.puts("存在しません。")
end
```

出力例:
```
存在します！
```
または、存在しない場合:
```
存在しません。
```

## Deep Dive (深いダイブ)
Elixirが内部的に使用する`File.dir?/1`関数は、Erlangの`:filelib.is_dir/1`に基づいています。これはElixirがErlangのVM（BEAM）上で構築されているためです。この関数はディレクトリが存在するかどうかを確認し、ブール値を返します。

代替手段としては、`File.stat/2` を使ってファイルのメタデータを取得し、それがディレクトリかどうかを確認する方法がありますが、`File.dir?/1` が最も簡潔で直接的です。

古いバージョンのElixirや他の言語では、ディレクトリ存在チェックの実装が異なりましたが、現在は多くの言語で同様のシンプルなAPIが提供されています。

## See Also (関連情報)
- Elixirの公式ドキュメント: [File module](https://hexdocs.pm/elixir/File.html)
- Erlangの公式ドキュメント: [filelib module](http://erlang.org/doc/man/filelib.html)
- ファイル操作のベストプラクティスについて: [The Pragmatic Programmer](https://pragprog.com/book/tpp/the-pragmatic-programmer) (書籍内のファイルI/Oの章)
