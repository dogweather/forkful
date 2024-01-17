---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "Elixir: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何かというと、なんで
ディレクトリの存在を確認するとは、プログラマーがファイルシステム内の特定のディレクトリが現在存在しているかどうかを確認することを意味します。プログラマーはこれを行うのは、アプリケーションの実行中に必要なファイルやディレクトリが存在するかどうかを確認するためです。

## 方法：
Elixirでは、Fileモジュールの`exists?/1`関数を使用してディレクトリの存在を確認することができます。以下は例です。
```Elixir
directory = "/usr/local/bin" # チェックするディレクトリのパス
if File.exists?(directory) do # File.exists?/1を使って存在を確認
  IO.puts("Directory exists!")
else
  IO.puts("Directory doesn't exist")
end
```

出力：
```
Directory exists!
```

## 深く掘り下げる
この機能は、以前のバージョンのElixirではファイルパスを2つのパラメーターとして受け取る`Elixir File.exists?/2`関数で実装されていました。しかし、現在のバージョンでは単一のパラメーターを受け取るように変更されました。また、`Elixir File.exists?/1`関数はファイルだけでなくディレクトリの存在もチェックすることができます。しかし、ファイルシステムを直接操作するのではなく、その代わりに`File.cwd/0`関数を使って現在の作業ディレクトリを取得することもできます。

## 関連情報を見る
- Elixir公式ドキュメント：https://hexdocs.pm/elixir/File.html#exists?/1
- ファイル操作に関するElixir講座（英語）：https://elixir-lang.org/getting-started/io-and-the-file-system.html