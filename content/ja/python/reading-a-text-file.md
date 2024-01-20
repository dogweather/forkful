---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何と何故？
テキストファイルの読み込みは、プログラムがテキストファイルからデータを読み取り、それを利用する行為を指します。しかし、なぜプログラマーはテキストファイルを読むのでしょうか？それは、テキストファイルからデータを取得して操作したり分析したりするためです。

## 実装方法：
以下は、Pythonを使用してテキストファイルを読み込むサンプルコードです。

```Python
# ファイルのオープン
file = open("sample.txt", "r")

# ファイルの読み込み
content = file.read()

# 読み取った内容の表示
print(content)

# ファイルのクローズ
file.close()
```

このスクリプトを実行すると、`sample.txt`ファイル内の全ての内容が表示されます。

## 深堀り
歴史的な文脈としては、テキストファイルというのは、人間が直接読むことができ、そしてプログラムでも扱いやすい形式であり、古くからデータの保存や交換のために使用されています。代替方法としては、XMLやJSONといったデータ構造も存在しますが、この二つは構造化データの交換のためにより適していると言えます。

テキストファイルの読み込みの実装詳細については、Pythonでは`open`関数を使ってファイルを開き、`read`メソッドで内容を読み取るというシンプルな方法が取られています。

## 参考資料
以下の連結は、より詳しくテキストファイルの扱い方について学習できる参考資料です：

Python 公式ドキュメンテーション - 入出力: https://docs.python.org/ja/3/tutorial/inputoutput.html#reading-and-writing-files

Pythonによるテキストファイルの読み書き（読み込み/書き込み）方法: https://note.nkmk.me/python-file-io-open-with/