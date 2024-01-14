---
title:                "Python: コンピュータプログラミング上読み込みコマンドライン引数"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜ

コマンドライン引数を読み取ることによって、あなたのPythonプログラミングスキルをさらに高めることができます。また、複雑なユーザー入力を必要とするプログラムを作成することもできます。この記事では、コマンドライン引数の読み取り方について説明します。

## ハウツー

Pythonでコマンドライン引数を読み取るのは非常に簡単です。まず、sysモジュールをインポートします。そして、sys.argvを使用して、コマンドライン引数がリストとして取得できます。

```Python
import sys

args = sys.argv
```

上記のコードでは、コマンドライン引数がargsという変数にリストとして格納されます。プログラムを実行する際に、引数を指定することができます。

```shell
python myprogram.py arg1 arg2
```

例えば、上記のようにコマンドラインで実行すると、argsのリストには['myprogram.py', 'arg1', 'arg2']という値が格納されます。

## ディープダイブ

コマンドライン引数を読み取る際に、特に注意するべき点は二つあります。

1. リストの最初の要素には実行ファイル名が格納される
2. 入力された引数は全て文字列として取得される

上記のコードを使えば、コマンドライン引数をリストとして取得できるため、簡単に操作することができます。例えば、特定の引数が指定されているかどうかを確認することができます。

```Python
import sys

args = sys.argv

if 'arg1' in args:
    # 引数が指定されている場合の処理
else:
    # 引数が指定されていない場合の処理
```

また、リストのスライスを使えば、必要な引数だけを抜き出すこともできます。

```Python
import sys

args = sys.argv

# 第二引数以降のみを抜き出す
arguments = args[1:]

# 第二引数のみを抜き出す
argument = args[1]
```

コマンドライン引数を読み取ることで、プログラムの柔軟性を高めることができます。ぜひ、試してみてください！

## 参考リンク

- Python公式ドキュメント: https://docs.python.org/ja/3/library/sys.html
- Real Pythonのチュートリアル: https://realpython.com/command-line-arguments-python/