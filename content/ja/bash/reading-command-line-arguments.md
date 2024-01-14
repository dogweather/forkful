---
title:    "Bash: コマンドライン引数を読む"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ？
Bashプログラミングをしている方々がコマンドライン引数を読み込むことに興味を持つ理由は何でしょうか？コマンドライン引数は、プログラムに対して柔軟性をもたらすために使用することができる重要な要素です。

## やり方
コマンドライン引数を読み込むには、まずは「$@」を使用して引数を取得することができます。そして、「$1」や「$2」を使用して、引数の位置を指定することができます。以下のコード例を参考にしてください。

```Bash
# 引数を取得する
echo "引数1: $1"
echo "引数2: $2"

# 実行結果
# 引数1: arg1
# 引数2: arg2
```

また、引数の数や特定の引数が与えられているかどうかをチェックすることもできます。以下のコード例を参考にしてください。

```Bash
# 引数の数が2つ以上かどうかをチェック
if [ $# -ge 2 ]; then
    echo "引数の数は2つ以上です。"
else
    echo "引数の数は1つ以下です。"
fi

# 特定の引数が与えられたかどうかをチェック
if [ "$1" = "-c" ]; then
    echo "引数-cが与えられました。"
fi
```

## ディープダイブ
コマンドライン引数を読み込む際に、必要な注意点があります。例えば、スペースや特殊文字が含まれる引数を正しく取得するためには、クォーテーションやエスケープを使用する必要があります。また、引数にデフォルト値を設定したり、引数をオプションとして扱う方法などもあります。コマンドライン引数をより詳しく理解するためには、さらに学習する必要があります。

## 他の参考資料
- [Bashコマンドライン引数のハンドリングに関するドキュメント](https://www.gnu.org/software/bash/manual/html_node/Using-Positional-Parameters.html)
- [Steve's Shell Scripting Tutorial - Command Line Arguments](https://www.shellscript.sh/functions.html)
- [LinuxHint - Bash Command Line Arguments](https://linuxhint.com/bash_command_line_arguments/)
- [UNIX Tutorial - Command Line Arguments](https://www.tutorialspoint.com/unix/unix-command-line-arguments.htm)
- [BashScripting.info - Positional parameters](https://www.bashscripting.info/oshp/chp-4-sect-1.html)