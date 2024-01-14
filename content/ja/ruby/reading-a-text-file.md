---
title:    "Ruby: テキストファイルを読む"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイル読み書きは、プログラミングの基本的なスキルの一つです。テキストファイルを読み書きすることで、大量のデータを管理したり、プログラムの実行環境に依存しないデータのやりとりが可能になります。

## 方法

まず、読み込むテキストファイルが存在するディレクトリを特定します。次に、Rubyの `File` クラスを使用してファイルを開きます。その後、`readlines` メソッドを使ってファイルの中身を一行ずつ読み込み、必要な処理を行います。最後に、ファイルを閉じます。

```Ruby
directory = "/Users/example/file.txt"
file = File.open(directory, "r")
lines = file.readlines
# necessary processing
file.close
```

## 深堀

テキストファイルの読み込みには、いくつかの異なる方法があります。上記の例では、一行ずつ読み込む方法を紹介しましたが、`read` メソッドを使うとファイルの全ての内容を一気に読み込むこともできます。また、読み込んだ内容を配列やハッシュに変換することも可能です。さらに、ファイルを書き込む方法もあります。

## 参考リンク

- [RubyのFileクラス公式ドキュメント](https://docs.ruby-lang.org/en/2.6.0/File.html)
- [テキストファイルを読み書きする方法 - codezine](https://codezine.jp/article/detail/12431)