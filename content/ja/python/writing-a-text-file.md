---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストファイルへの書き込みは、データを永続的に保存するプロセスです。プログラマーは設定、データの出力、ログ作成のためにこれを行います。

## How to: (方法)
```Python
# ファイルを開いて書き込む
with open('sample.txt', 'w', encoding='utf-8') as f:
    f.write("Pythonは楽しい！")

# ファイルを読んで確認する
with open('sample.txt', 'r', encoding='utf-8') as f:
    print(f.read())
```
出力:
```
Pythonは楽しい！
```

## Deep Dive (詳細情報)
初期のコンピューターシステムでは、テープやパンチカードを使いデータを保存していましたが、テキストファイルは情報の柔軟な取扱いを可能にしました。`open()`関数には`'w'`モード以外に`'a'`（追記）や`'x'`（存在しない場合にのみ作成）などがあります。`'w'`はファイルが既に存在する場合、内容を上書きしますので注意が必要です。ファイルI/Oの性能は、書き込むデータの量やディスクの種類に大きく依存します。

## See Also (関連情報)
- Python公式ドキュメント: https://docs.python.org/ja/3/tutorial/inputoutput.html#reading-and-writing-files
- W3Schools Python File Handling: https://www.w3schools.com/python/python_file_handling.asp
- Real Python記事（ファイルI/Oガイド）: https://realpython.com/read-write-files-python/
