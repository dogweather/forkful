---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
テキストファイルの書き込みとは、文字データをファイルに保存することです。プログラマーはデータの永続化、ログ記録、設定の保存などのためにこれを行います。

## How to:
```Fish Shell
# テキストファイル "sample.txt" に "Hello, World!" を書き込む
echo "Hello, World!" > sample.txt

# 内容確認
cat sample.txt
```
出力:
```
Hello, World!
```
```Fish Shell
# ファイルに複数行を追加
for line in "First line" "Second line" "Third line"
    echo $line >> sample.txt
end

# 内容確認
cat sample.txt
```
出力:
```
Hello, World!
First line
Second line
Third line
```

## Deep Dive
テキストファイルの書き込みはUNIX系OSでは古くから使われています。Fish Shellの書き込みもUNIXの流れを汲んでおり、シンプルで強力です。`>`は新規/上書き、`>>`は追記モードです。代替として`tee`コマンドやスクリプト言語内のファイル操作関数が存在します。ファイルIOの実装はシステムコールに依存しており、OSのファイルシステムと密接に関わっています。

## See Also
- [Fish Documentation on Input/Output Redirection](https://fishshell.com/docs/current/index.html#syntax-redirection)
- [GNU Coreutils Documentation](https://www.gnu.org/software/coreutils/)
