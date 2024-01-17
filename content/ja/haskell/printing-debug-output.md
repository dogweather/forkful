---
title:                "「デバッグ出力の印刷」"
html_title:           "Haskell: 「デバッグ出力の印刷」"
simple_title:         "「デバッグ出力の印刷」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何？それが何であり、プログラマーがなぜそれをするのか

デバッグ出力のプリントとは、プログラムの実行中に発生した情報やエラーメッセージをコンソールやファイルに出力することを指します。プログラマーはこの出力を使用して、プログラムの実行中に何が起こっているかを把握し、問題を特定して修正することができます。

## 方法：

Haskellでは、 ```print```関数を使用してデバッグ出力をプリントすることができます。

例：

```Haskell
num1 :: Int
num1 = 5

num2 :: Int
num2 = 10

print (num1 + num2)
```

出力：
```Haskell
15
```

また、特定の条件でのみ出力するように制御することもできます。例えば、特定の変数の値がある値よりも大きい場合にのみ出力するようにするには、以下のようにします。

```Haskell
num :: Int
num = 20

if num > 10
  then print "Num is bigger than 10"
  else print "Num is smaller than or equal to 10"
```

出力：
```Haskell
 "Num is bigger than 10"
```

## 詳細：

デバッグ出力は、プログラミング言語の歴史が古い時代から使用されてきました。初期のプログラミング言語では、コンソールに直接メッセージを出力することが一般的でしたが、近年ではファイルに出力することも可能になりました。

Haskell以外にも、他のプログラミング言語でも同様のデバッグ出力機能が利用できます。例えば、Pythonでは ```print```関数を使用して出力することができます。

デバッグ出力の実装は、基本的にはプログラミング言語の標準ライブラリに含まれていますが、コンソールやファイルに出力する方法はプログラミング言語ごとに異なります。

## 関連リンク：

- [Haskellの公式ドキュメント](https://www.haskell.org/documentation/)
- [Pythonの公式ドキュメント](https://www.python.org/doc/)
- [プログラミング入門サイト「ドットインストール」のPython講座](https://dotinstall.com/lessons/basic_python_v2)