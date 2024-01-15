---
title:                "テキストファイルを書く"
html_title:           "Python: テキストファイルを書く"
simple_title:         "テキストファイルを書く"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

##なぜ
テキストファイルを書くことのメリットを説明します。
テキストファイルを書くことにより、プログラムの出力やデータの保存が簡単になります。また、他のプログラムで読み込みやすく、様々な用途に活用することができます。

##やり方
テキストファイルを書くにはPythonの `open()` 関数を使用します。以下のようにコードブロックで示します。

```Python
# テキストファイルを書き込みモードでオープン
file = open("sample.txt", "w")

# テキストファイルに"Hello, World!"を書き込み
file.write("Hello, World!")

# ファイルをクローズ
file.close()

# 作成されたファイルを読み込んで出力
new_file = open("sample.txt", "r")
print(new_file.read())

#=> "Hello, World!"
```

##深堀り
テキストファイルを書く際、オープンするモードにより挙動が異なります。`"w"`モードでオープンすると、既存のファイルがあった場合は上書きされ、ファイルが存在しない場合は新しく作成されます。また、ファイルをクローズすることでメモリの解放やデータの保存が行われます。

また、ファイルをオープンした後は、ファイルオブジェクトのメソッドを使用してファイルへの書き込みや読み込みを行います。例えば、`write()` メソッドを使用することでテキストを書き込むことができます。

##参考リンク
- [Python入門 (基本文法編)](https://www.python.jp/train/basic/intro.html)
- [Pythonのopen関数について](https://qiita.com/kgsi/items/fb6a281f957e96d65e0d)