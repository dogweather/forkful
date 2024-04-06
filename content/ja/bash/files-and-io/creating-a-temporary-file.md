---
date: 2024-01-20 17:39:45.235534-07:00
description: "How to: (\u65B9\u6CD5) \u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\
  \u308B\u6700\u3082\u4E00\u822C\u7684\u306A\u65B9\u6CD5\u306F `mktemp` \u30B3\u30DE\
  \u30F3\u30C9\u3092\u4F7F\u3046\u3053\u3068\u3067\u3059\u3002\u4F8B\u3092\u898B\u3066\
  \u307F\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.230209-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\u4F5C\u308B\u6700\
  \u3082\u4E00\u822C\u7684\u306A\u65B9\u6CD5\u306F `mktemp` \u30B3\u30DE\u30F3\u30C9\
  \u3092\u4F7F\u3046\u3053\u3068\u3067\u3059\u3002\u4F8B\u3092\u898B\u3066\u307F\u307E\
  \u3057\u3087\u3046\u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

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
