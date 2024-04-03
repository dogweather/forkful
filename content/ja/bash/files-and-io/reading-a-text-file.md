---
date: 2024-01-20 17:53:49.970099-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3063\
  \u3066\u306E\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u8868\u793A\
  \u3057\u305F\u308A\u3001\u51E6\u7406\u3059\u308B\u3053\u3068\u3060\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u306F\u30C7\u30FC\u30BF\u89E3\u6790\u3001\u30B9\u30AF\u30EA\u30D7\
  \u30C8\u8A2D\u5B9A\u3001\u81EA\u52D5\u5316\u3068\u3044\u3063\u305F\u30BF\u30B9\u30AF\
  \u306B\u5FC5\u8981\u3060\u304B\u3089\u3001\u30D5\u30A1\u30A4\u30EB\u8AAD\u307F\u8FBC\
  \u307F\u3092\u4F7F\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.398686-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3063\
  \u3066\u306E\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u8868\u793A\
  \u3057\u305F\u308A\u3001\u51E6\u7406\u3059\u308B\u3053\u3068\u3060\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u306F\u30C7\u30FC\u30BF\u89E3\u6790\u3001\u30B9\u30AF\u30EA\u30D7\
  \u30C8\u8A2D\u5B9A\u3001\u81EA\u52D5\u5316\u3068\u3044\u3063\u305F\u30BF\u30B9\u30AF\
  \u306B\u5FC5\u8981\u3060\u304B\u3089\u3001\u30D5\u30A1\u30A4\u30EB\u8AAD\u307F\u8FBC\
  \u307F\u3092\u4F7F\u3046\u3002."
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
