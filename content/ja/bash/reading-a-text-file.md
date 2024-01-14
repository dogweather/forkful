---
title:                "Bash: テキストファイルの読み込み"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ
テキストファイルを読むために、読者がこの記事を読む理由を1〜2文で説明します。

プログラミングやシステム管理をしていると、テキストファイルを読み取る必要があることがあります。例えば、設定ファイルやログファイルを読む必要がある場合があります。そこで、この記事ではBashでテキストファイルを読む方法を紹介します。

## ハウトゥー
まず、テキストファイルを読むにはcatコマンドを使用します。以下のように入力します。

```Bash
cat file.txt
```

これで、file.txtというファイルの中身が表示されます。また、指定したファイル以外にも複数のファイルを読み込むことができます。例えば、以下のように入力します。

```Bash
cat file1.txt file2.txt
```

これで、file1.txtとfile2.txtの中身が順に表示されます。

また、ファイルを行単位で読み込むこともできます。例えば、以下のように入力します。

```Bash
cat -n file.txt
```

これで、file.txtの行番号が付いて表示されます。さらに、grepコマンドを使用することで、特定の文字列を含む行のみを抽出することもできます。

## ディープダイブ
catコマンドの他にも、テキストファイルを読む方法はいくつかあります。例えば、lessやmoreコマンドを使用することで、ファイルの中身をスクロールしながら表示することができます。また、sedやawkといったコマンドを使用してファイルの内容を変更することもできます。

## 参考リンク
- [Bashユーザーズガイド](https://files.fosswire.com/2007/bash.pdf)
- [Linuxコマンド集](http://linuxcommand.org/index.php)
- [Bashチュートリアル](https://linuxconfig.org/bash-scripting-tutorial)

## 参考文献
- [Bashコマンドラインチェットシート](https://devhints.io/bash)
- [Linuxコマンドの基本的な使い方](https://www.atmarkit.co.jp/ait/articles/1801/22/news007.html)
- [コマンドラインでテキストファイルを編集する方法](https://qiita.com/ryo0301/items/d321a6e6f842d6583e74)

ありがとうございました。Happy coding!