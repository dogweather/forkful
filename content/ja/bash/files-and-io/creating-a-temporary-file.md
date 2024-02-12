---
title:                "一時ファイルの作成"
aliases: - /ja/bash/creating-a-temporary-file.md
date:                  2024-01-20T17:39:45.235534-07:00
model:                 gpt-4-1106-preview
simple_title:         "一時ファイルの作成"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜか？)
一時ファイルは短期間データを保存するためのファイルです。スクリプトが処理中に一時的なデータを扱う必要がある時、安全かつ効率的に操作するために使います。

## How to: (方法)
一時ファイルを作る最も一般的な方法は `mktemp` コマンドを使うことです。例を見てみましょう。

```Bash
# 一時ファイルを作成し、変数にパスを保存
temp_file=$(mktemp)

# 一時ファイルにデータを書き込む
echo "This is a temporary file" > "$temp_file"

# 一時ファイルの内容を表示
cat "$temp_file"

# 一時ファイルを削除
rm "$temp_file"
```

出力はこんな感じになります。

```
This is a temporary file
```

## Deep Dive (深めの情報)
`mktemp` コマンドが追加されたのは、偶然や予想外のファイル名の衝突を防ぐためです。以前は、コーダーが自分で一時ファイル名を生成したりしていましたが、これはセキュリティリスクがありました。`mktemp` は毎回独特で予測不能なファイル名を生成してくれるんです。

一時ファイル以外には、`/dev/shm` やメモリベースのファイルシステムを使用する方法もありますが、これらは時々リソースに限りがあったり、特定の要件がある場合に適しています。

`mktemp` コマンドは `--tmpdir` オプションで特定のディレクトリに一時ファイルを生成したり、テンプレートを使ってファイル名の一部を指定することができます。しかし、基本的な使用では、上記の方法で十分です。

## See Also (関連情報)
- GNU Coreutils `mktemp` のマニュアルページ: https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html
- Advanced Bash-Scripting Guide (一時ファイルに関する章): https://www.tldp.org/LDP/abs/html/x9644.html
- Linux Journal でのセキュリティと一時ファイルについての記事: https://www.linuxjournal.com/article/6701
