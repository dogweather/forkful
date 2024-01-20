---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？
一時ファイル作成は、プログラムが実行中に短時間データを保管するための方法です。これは、大量のデータをプログラム間で高速に移動したり、時間とともに変化するデータを追跡したりするために使用されます。

## 作り方：
```Bash 
# tempファイル作成
temp_file=$(mktemp)

# tempファイルに何かを書く
echo "Hello, World!" > $temp_file

# tempファイルの内容を出力する
cat $temp_file

# tempファイルを削除する
rm $temp_file
```
このスクリプトの結果：
```Bash 
Hello, World!
```

## ディープダイブ：
一時ファイルの仕組みはUnix系システムの初期の段階から存在しました。ただし、`mktemp`は後に追加され、一時ファイルの安全な作成を可能にしました。しばしば一時ファイルの代わりにRAMディスクやメモリマップファイルが使用されます。これらは一時ファイルと同じ目的を果たしますが、さまざまな利点（高速なアクセス時間）と欠点（制限されたサイズ）があります。

具体的な実装については、一時ファイルは通常、`/tmp`ディレクトリ内に作成され、名前はランダムに生成されます。これにより、同じスクリプトが複数回実行されてもファイル名の衝突が防げます。

## 参照リンク：
- [`mktemp` man page（英語）](https://linux.die.net/man/1/mktemp)
- [Temporary file Wikipedia（日本語）](https://ja.wikipedia.org/wiki/%E4%B8%B2%E6%99%82%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB)
- [Bash programming guide（英語）](https://www.tldp.org/LDP/abs/html/)