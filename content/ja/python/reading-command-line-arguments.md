---
title:                "コンピュータプログラミングの記事タイトル： コマンドライン引数の読み取り"
html_title:           "Python: コンピュータプログラミングの記事タイトル： コマンドライン引数の読み取り"
simple_title:         "コンピュータプログラミングの記事タイトル： コマンドライン引数の読み取り"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

何をしたくてコマンドライン引数を読み込むのか、そしてプログラマーがそれをする理由を教えてあげましょう。

## What & Why?

コマンドライン引数を読み込むことは、プログラムに外部からの入力を提供することです。これにより、プログラムに対して実行時に値を与えることができ、より柔軟なアプリケーションを作ることができます。プログラマーは、ユーザーによる入力を受け取ることなく、プログラムを実行させたい場合にコマンドライン引数を読み込むことがあります。

## How To:

```Python
import sys
# コマンドライン引数を読み込み、リストとして受け取る
args = sys.argv
# コマンドライン引数の値を取得する
arg1 = args[1]
arg2 = args[2]
# 出力する
print(arg1, arg2)
```

### 入力例:
```
python my_script.py hello world
```

### 出力結果:
```
hello world
```

## Deep Dive:

コマンドライン引数の機能は、1960年代から存在しています。当時は、コンピュータが大きくて高価で、複数のユーザーが同じ機械を共有する必要がありました。そのため、複数のプログラムを同時に実行することが難しかったため、プログラムに引数を渡すことで、より効率的に複数のタスクを実行することができるようになりました。

コマンドライン引数の代替方法として、環境変数やコンフィグファイルなどがありますが、簡単に構築できることやコマンドラインから直接値を渡すことができることから、コマンドライン引数がよく使われます。

コマンドライン引数の実装方法は言語によって異なりますが、Pythonでは```sys.argv```を使用してコマンドライン引数を受け取ります。また、外部ライブラリを使用することで、より高機能なコマンドライン引数の取得や解析ができるようになります。

## See Also:

- [実践 Python3](https://www.amazon.co.jp/%E5%AE%9F%E8%B7%B5Python3-%E5%88%9D%E5%BF%83%E8%80%85%E3%81%AE%E3%81%9F%E3%82%81%E3%81%AE%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0%E6%B3%95-Guido-van-Rossum/dp/4873117380/ref=sr_1_1?adgrpid=51814479271&dchild=1&gclid=Cj0KCQjw59n8BRD2ARIsAAmgPmIn9veTvuZ_cvHdBqXRwivzZlKQK5zH8jYLSdyMC8G5Lzi-U0HH0fQaAvX4EALw_wcB&hvadid=338545468244&hvdev=c&hvlocphy=1009283&hvnetw=g&hvqmt=e&hvrand=1529485291735943074&hvtargid=kwd-30550818&hydadcr=12152_11218045&jp-ad-ap=0&keywords=%E5%AE%9F%E8%B7%B5python&qid=1626011731&sr=8-1&tag=googhydr-22)
- [Python documenation on sys module](https://docs.python.org/3/library/sys.html)
- [Argument Parser library for Python](https://docs.python.org/3/library/argparse.html)