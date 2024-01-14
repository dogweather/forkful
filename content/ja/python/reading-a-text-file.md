---
title:                "Python: テキストファイルの読み込み"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

なぜ

## なぜ

テキストファイルを読むことには、多くの理由があります。例えば、テキストファイルには重要なデータや情報が含まれている場合があり、それをコンピューターで簡単にアクセスすることができるからです。また、プログラムの学習や開発においてもテキストファイルを読むことは重要です。

## 読み込む方法

Pythonを使用すると、プログラムの中でテキストファイルを簡単に読み込むことができます。まず、ファイルを開くための ```open()``` 関数を使用します。その後、ループを使用してテキストファイルの各行を読み込み、必要な処理を行います。最後に、ファイルを閉じることを忘れないようにしましょう。

```
file = open("textfile.txt", "r")  # ファイルを読み込みモードで開く
for line in file:
  print(line)  # 各行を出力する
file.close()  # ファイルを閉じる
```

上記の例では、 ```textfile.txt``` という名前のテキストファイルを読み込みモードで開いています。ループを使って各行を順番に読み込み、 ```print()``` 関数を使って出力しています。最後に、ファイルを閉じていることに注目してください。

## より詳細な情報

テキストファイルの読み込みにはさまざまな手法があります。例えば、ファイルの特定の行を読み込む方法や、ファイル内の特定の文字列を検索する方法などがあります。また、テキストファイルのデータを加工して使用する方法や、ファイルを書き込む方法についても学ぶことができます。

## 関連リンク

- [Pythonの公式ドキュメント](https://docs.python.org/ja/3/tutorial/inputoutput.html)
- [プログラミング初心者が知っておきたいテキストファイルの操作方法](https://paiza.hatenablog.com/entry/2018/02/09/%E3%83%86%E3%82%AD%E3%82%B9%E3%83%88%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB)
- [テキストファイルを扱う方法 (Python実践入門)](https://www.python.jp/train/text/index.html)