---
title:                "ディレクトリが存在するかどうかを確認する"
date:                  2024-01-19
html_title:           "Bash: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)
ディレクトリが存在するかどうかを確認することは、ファイル操作の前に予期せぬエラーを防ぐためです。プログラムがスムーズに流れるよう、確実性を持たせる重要なステップです。

## How to: (方法)
```Bash
# ディレクトリが存在するかチェックする
if [ -d "$DIRECTORY" ]; then
  echo "$DIRECTORY exists."
else
  echo "$DIRECTORY does not exist."
fi

# 実行結果
# /path/to/dir exists.
# または
# /path/to/dir does not exist.
```

## Deep Dive (深掘り)
ディレクトリが存在するかどうかのチェックはUNIX由来の機能で、初期のコンピューターシステムから続いています。`test`コマンド(`[`とも書きます)の`-d`オプションを利用します。この方法は古典的ですが、効果的です。また、Bashスクリプトでは`[[ -d $DIRECTORY ]]`のように二重角括弧を使うことも可能です。`mkdir -p`コマンドを使い、存在しない場合は作成することもできます。

## See Also (関連情報)
- Bashマニュアル: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
