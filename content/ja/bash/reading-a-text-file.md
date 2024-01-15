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

## なぜ

テキストファイルを読み取ることの重要性は、システム管理者やプログラマーなどのコンピューター関連の仕事にとって不可欠です。テキストファイルには様々な設定やデータが保存されており、必要に応じてその内容を読み取ることでシステムの動作や問題の解決に役立ちます。

## ハウトゥー

Bashを使用してテキストファイルを読み取る方法を見ていきましょう。

```Bash
# テキストファイルの内容を表示する
cat file.txt

# 特定の行のみを表示する
sed -n '2p' file.txt # 2行目の内容を表示

# キーワードを含む行のみを表示する
grep "keyword" file.txt

# ファイルの特定部分を表示する
head -5 file.txt # 先頭から5行までを表示
tail -2 file.txt # 最後の2行を表示
```

上記のコマンドを実行することで、テキストファイルの内容を見ることができます。また、それぞれのコマンドにオプションを追加することで、さらに細かな制御が可能です。

## ディープダイブ

テキストファイルを読み取る際によく使われるコマンドには、`awk`や`cut`などがあります。これらのコマンドを使用することで、テキストファイルの特定部分を抽出したり、より複雑な処理を行うことができます。

また、一般的なテキストエディターの中にも、テキストファイルを読み取る機能が備わっています。例えば、Vimを使用する場合は、`vim file.txt`と入力することでファイルを開くことができます。ただし、テキストエディターを使用する場合は、コマンドラインの操作に比べると学習コストが高いため、慣れていない方は注意が必要です。

## シー・アルソウ

- [Bashでのテキスト処理コマンドの使い方](https://www.atmarkit.co.jp/ait/articles/1901/31/news007.html)
- [Linuxにおけるテキスト処理の基礎](https://www.infraexpert.com/study/linuxbasic3.html)

---

## シー・アルソウ

- [Bashでのテキスト処理コマンドの使い方](https://www.atmarkit.co.jp/ait/articles/1901/31/news007.html)
- [Linuxにおけるテキスト処理の基礎](https://www.infraexpert.com/study/linuxbasic3.html)