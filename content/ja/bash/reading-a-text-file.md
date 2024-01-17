---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何となぜ

テキストファイルを読むこととは、プログラマーがプログラムを作るために、ファイル内のテキストをプログラムに組み込むことです。この作業は、プログラムがファイルから必要な情報を取得し、処理するために必要です。

## 方法：

```Bash
# テキストファイルを読み取り、行ごとに表示する
cat sample.txt

# テキストファイル内の特定の文字列を検索する
grep "keyword" sample.txt

# テキストファイル内のデータを変数に格納する
data=$(cat sample.txt)

# テキストファイルから情報を読み取り、新しいファイルに書き込む
cat sample.txt > newfile.txt

# テキストファイルを別のコマンドで処理する
cat sample.txt | sed 's/before/after/'
```

## 詳細を見る

テキストファイルを読み取る手法は、数多く存在しますが、Bashを使用することでシンプルかつ直感的に実行することができます。また、代替手段としてPerlやPythonを使用することもできますが、Bashは標準でインストールされており、学習コストが低いため、初心者にとっては便利です。Bashは、80年代にUnixシステムで開発されたシェルスクリプト言語です。

## 関連リンク

- [Bashのチュートリアル](https://www.w3schools.com/whatis/whatis_bash.asp)
- [Bashの公式ドキュメント](https://www.gnu.org/software/bash/)
- [Bashの歴史](https://en.wikipedia.org/wiki/Bash_(Unix_shell))