---
title:                "Python: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜテキストファイルを書くのか

テキストファイルはプログラミングにおいて、データや情報を保存するために重要な役割を果たします。また、読み取りや編集が容易であり、ソースコードや設定ファイルとしても使用されます。

## テキストファイルの書き方

次のPythonコードブロックには、テキストファイルを書き込むための簡単な例が含まれています。

```Python
# ファイルを書き込みモードで開く
text_file = open("example.txt", "w")

# テキストをファイルに書き込む
text_file.write("こんにちは！これはテストのテキストです。")

# ファイルを閉じる
text_file.close()

# ファイルを読み取りモードで開く
text_file = open("example.txt", "r")

# テキストを読み込む
text = text_file.read()

# ファイルを閉じる
text_file.close()

# テキストを出力する
print(text)
```

出力結果:

```
こんにちは！これはテストのテキストです。
```

## テキストファイルの詳細

テキストファイルを書き込むためには、ファイルを開き、テキストを書き込み、ファイルを閉じるという基本的な手順に従います。また、ファイルを読み取る際にはファイルを開いてから読み取り、最後にファイルを閉じるというプロセスを行います。

また、テキストファイルをより複雑な形式で書き込むことも可能です。例えば、CSVファイルやJSONファイルなどのように、データをより構造化して保存することができます。

## 参考リンク

- Pythonの公式ドキュメント: https://docs.python.org/ja/3/tutorial/inputoutput.html#reading-and-writing-files
- テキストファイルのバイナリモードについて: https://python.keicode.com/lang/file-binary.php
- CSVファイルを書き込む方法: https://note.nkmk.me/python-csv-writer/