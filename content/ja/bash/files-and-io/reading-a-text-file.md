---
date: 2024-01-20 17:53:49.970099-07:00
description: "How to: (\u3084\u308A\u65B9) Bash\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\
  \u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u306E\u306F\u7C21\u5358\u3002`cat`, `less`,\
  \ `head`, `tail`, `grep`\u3068\u3044\u3063\u305F\u30B3\u30DE\u30F3\u30C9\u3067\u3044\
  \u3051\u308B\u3002\u4EE5\u4E0B\u306B\u4F8B\u3092\u793A\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.398686-06:00'
model: gpt-4-1106-preview
summary: "Bash\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\
  \u306E\u306F\u7C21\u5358\u3002`cat`, `less`, `head`, `tail`, `grep`\u3068\u3044\u3063\
  \u305F\u30B3\u30DE\u30F3\u30C9\u3067\u3044\u3051\u308B\u3002\u4EE5\u4E0B\u306B\u4F8B\
  \u3092\u793A\u3059."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

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
