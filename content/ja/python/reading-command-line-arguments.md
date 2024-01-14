---
title:    "Python: コマンドライン引数の読み込み"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜ
コマンドライン引数を読み取ることの重要性について説明します。コマンドライン引数を使用することで、プログラムの実行時に複数のパラメーターを指定することができます。これにより、プログラムの柔軟性が向上し、ユーザーが入力する値を事前に指定することができます。

## やり方
Pythonを使用してコマンドライン引数を読み取る方法を説明します。以下のコードブロックを使用して、コマンドライン引数を読み取り、出力する簡単なプログラムを示します。

```python
# コマンドライン引数を読み取る方法
import sys

# sys.argvを使用してコマンドライン引数を取得
args = sys.argv[1:] # 最初の引数(プログラム名)を除外する

# 入力された引数を出力
print("入力された引数は以下の通りです：")
for arg in args:
    print(arg)
```

このコードを実行すると、プログラム名を除いたコマンドライン引数が出力されます。例えば、`python my_program.py hello world`というコマンドを実行すると、以下のような出力が得られます。

```
入力された引数は以下の通りです：
hello
world
```

## ディープダイブ
コマンドライン引数にはさまざまなオプションがあります。例えば、`-a`や`--option`のようなフラグを使用して、特定の動作を実行することができます。また、引数には値を指定することもできます。例えば、`--name="John"`のように、`--name`という引数に`John`という値を指定することができます。

コマンドライン引数を使用することで、プログラムをよりカスタマイズできるようになります。ユーザーが入力する値によって、プログラムの動作が変化するように設計することができます。

# さらに見る
- [Python 公式ドキュメント - コマンドラインと環境変数](https://docs.python.org/ja/3/using/cmdline.html)
- [Pythonプログラミング：第一歩 - コマンドライン引数を取得する方法](https://pythonprogramming.net/command-line-arguments-argv/?completed=/modules-importing-modules-python-tutorial/)
- [プロクラシエス株式会社 - オプション付きのコマンドライン引数を読み取る方法](https://www.proclarus.co.jp/blog/python/python%E3%81%A7%E3%82%AA%E3%83%97%E3%82%B7%E3%83%A7%E3%83%B3%E4%BB%98%E3%81%8D%E3%81%AE%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E3%83%A9%E3%82%A4%E3%83%B3%E5%BC%95%E6%95%B0%E3%82%92%E8%AA%AD%E3%81%BF%E5%8F%96%E3%82%8B%E6%96%B9%E6%B3%95/)