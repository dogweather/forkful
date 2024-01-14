---
title:                "Python: コンピュータプログラミングの記事名：コマンドライン引数の読み取り"
simple_title:         "コンピュータプログラミングの記事名：コマンドライン引数の読み取り"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

Pythonを使ったプログラミングをする際、コマンドライン引数を読み取ることは非常に重要です。コマンドライン引数を正しく読み取ることにより、より動的なプログラムを作成することができます。

## 方法

コマンドライン引数を読み取るために、sysモジュールを使用します。以下のようにコードを書くことで、コマンドライン引数を読み取ることができます。

```Python
import sys

# Pythonファイル名と引数を格納するリストを作成
args = sys.argv

# 引数を取得してプログラムに応じた処理を実行する
if len(args) == 2:
    # 引数が1つの場合の処理
    print("こんにちは、{}さん！".format(args[1]))
else:
    print("こんにちは、名無しのエンジニアさん！")
```

上記の例では、コマンドラインで引数を指定することで、プログラムが動的に振る舞うようになります。例えば、コマンドラインで"python hello.py John"と入力することで、"こんにちは、Johnさん！"という出力を得ることができます。

## 詳細を掘り下げる

コマンドライン引数を読み取る方法には様々なバリエーションがあります。例えば、argparseモジュールを使用することで、より複雑なコマンドライン引数を受け取ることができます。また、コマンドライン引数を使用する際にはエラー処理も重要です。例えば、ユーザーが引数を間違って入力しても、エラーメッセージを出してプログラムを正しく動かすようにすることができます。

## See Also

- [Pythonの公式ドキュメント - sysモジュール](https://docs.python.org/ja/3/library/sys.html)
- [Pythonの公式ドキュメント - argparseモジュール](https://docs.python.org/ja/3/howto/argparse.html)
- [Pythonの公式ドキュメント - エラー処理](https://docs.python.org/ja/3/tutorial/errors.html)