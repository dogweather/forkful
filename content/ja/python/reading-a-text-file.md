---
title:                "「テキストファイルの読み込み」"
html_title:           "Python: 「テキストファイルの読み込み」"
simple_title:         "「テキストファイルの読み込み」"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何かもしらについて
テキストファイルを読むとは、テキスト編集ソフトで作成されたファイルの内容をプログラムで読み取ることです。プログラマーたちは、この方法を使ってテキストファイルから情報を取得し、それをプログラムに組み込むことができます。

## 方法：
```Python
file = open("sample.txt", "r") # ファイルを読み込む
content = file.read() # ファイルの中身を読み取る
print(content) # ファイルの内容を出力
file.close() # ファイルを閉じる
```

出力：
```
これはテキストファイルのサンプルです。
```

## 詳細情報：
- テキストファイルの読み取りは、プログラミングの世界で古くから使われている方法です。コンピューターが発展する前から存在していたテキストファイルは、今でも多くのプログラムで使用されています。
- テキストファイルを読み取るには、他にも方法があります。例えば、CSVファイルなど、特定の形式に従って書かれたファイルを読み取る方法です。
- テキストファイルを読み取る際には、文字コードなどの詳細な情報を指定する必要があります。また、ファイルを閉じることも重要です。

## 関連情報：
- [Pythonのドキュメント](https://docs.python.org/ja/3/library/functions.html?highlight=open#open)
- [Pythonチュートリアル](https://docs.python.org/ja/3/tutorial/inputoutput.html#reading-and-writing-files)