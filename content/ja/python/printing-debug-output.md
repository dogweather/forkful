---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何と何のため？
デバッグ出力のプリントは、コードの特定部分で何が起こっているかを追跡するためのプログラマの手段です。このプラクティスは、バグを特定し、問題の解決を早めるため、特に価値があります。

## 使い方:
Pythonでは`print`関数を使ってデバッグ出力を作成できます。以下に例を示します:

```Python
def add_two_numbers(num1, num2):
    result = num1 + num2
    print(f'Adding {num1} and {num2}. Result is {result}.')
    return result

add_two_numbers(5, 7)
```
このコードを実行すると、以下の出力が得られます:

```
Adding 5 and 7. Result is 12.
```

## 詳しい情報:
1. **歴史的背景:** `print`文はPythonのバージョンが進化するにつれて変化してきました。Python 2では、`print`は文でしたが、Python 3では`print()`として機能する関数になりました。
2. **代替手段:** 一部の開発者は、デバッガを用いることでデバッグ出力のプリントを回避します。デバッガの使用はより洗練された手法とされていますが、`print()`はその利便性から一般的に利用されています。
3. **内部の詳細:** `print()`関数は、内部では`sys.stdout.write()`を使用しています。これにより、出力はすぐにコンソールに表示され、バッファリングが遅延することを防いでいます。

## 追加情報:
- [Python公式ドキュメンテーションのprint()関数](https://docs.python.org/ja/3/library/functions.html#print)
- [Python Debugger](https://docs.python.org/ja/3/library/pdb.html) を使用したデバッグテクニックについて
- ロギングライブラリの[Python公式ドキュメンテーション](https://docs.python.org/ja/3/library/logging.html)。ログを使って更に洗練されたデバッグ出力を作成する方法について毎日のプログラミングに役立つ情報が記載されています。