---
title:                "ディレクトリが存在するかどうかの確認"
date:                  2024-01-20T14:56:25.489175-07:00
simple_title:         "ディレクトリが存在するかどうかの確認"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ディレクトリが存在するかチェックすることとは、ファイルシステム上の特定の場所に特定のフォルダがあるかどうかを確認するプロセスです。このチェックはスクリプトがエラーなく動作するため、または必要なリソースが利用可能かを保証するために行われます。

## How to: (やり方)
Fish Shellでディレクトリの存在を確認するには `test` コマンドを使います。以下の例と出力をご覧ください。

```Fish Shell
if test -d /path/to/directory
    echo "存在する"
else
    echo "存在しない"
end
```

サンプル出力:

```
存在する
```

または、ディレクトリが存在しない場合:

```
存在しない
```

## Deep Dive (深掘り)
Fish Shellでは、他のシェル同様に`test`コマンドを使ってファイルやディレクトリの状態をチェックすることができます。 `-d` オプションはディレクトリ専用です。Fishの歴史の中で、`[ ]`や`[[ ]]`の代わりに`test`が好まれるようになりました。これは、`test`が組み込みコマンドとして提供されており、POSIX準拠であるためです。また、`and` や `or` を使って複雑な条件を構成することも可能です。他に `stat` コマンドを用いても同様のチェックを行うことができますが、`test`の方が一般的にはシンプルで広く使われています。

## See Also (関連情報)
- [Fish Shellのドキュメント](https://fishshell.com/docs/current/index.html)
- [POSIX test コマンド](https://pubs.opengroup.org/onlinepubs/009695399/utilities/test.html)
- [Filesystem Hierarchy Standard](https://en.wikipedia.org/wiki/Filesystem_Hierarchy_Standard)
