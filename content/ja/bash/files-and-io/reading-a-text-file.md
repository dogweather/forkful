---
title:                "テキストファイルの読み込み"
aliases:
- /ja/bash/reading-a-text-file.md
date:                  2024-01-20T17:53:49.970099-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストファイルを読むってのは、ファイルの内容を表示したり、処理することだ。プログラマはデータ解析、スクリプト設定、自動化といったタスクに必要だから、ファイル読み込みを使う。

## How to: (やり方)
Bashでテキストファイルを読むのは簡単。`cat`, `less`, `head`, `tail`, `grep`といったコマンドでいける。以下に例を示す。

```Bash
# ファイル全体を表示
cat example.txt

# 最初の10行だけ表示
head example.txt

# 最後の10行だけ表示
tail example.txt

# ファイル内の'error'という文字列を含む行を検索
grep 'error' example.txt
```

サンプル出力:
```
Hello World
This is a sample text file for Bash tutorial.
```

## Deep Dive (深掘り)
テキストファイルの読み込みはUNIX系システムで長らく使われている。`cat`コマンドは1971年からある！`grep`など新しいツールは検索やパターンマッチングを高効率で行える。`awk`や`sed`はさらに高度なテキスト処理に使われる。ファイルの大きさによって、適切なコマンドを選ぶのがベストプラクティス。

## See Also (関連情報リンク)
- Bash scriptingのチュートリアル: https://www.gnu.org/software/bash/manual/
- UNIXコマンドの解説: https://www.tutorialspoint.com/unix_commands/cat.htm
- テキスト処理とデータ抽出について: https://www.grymoire.com/Unix/Sed.html
