---
title:                "Ruby: テキストファイルの作成"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを書くことの利点は多々あります。例えば、プログラムやスクリプトで必要なデータを保存することができたり、ログファイルに出力することでエラーの追跡がしやすくなったりします。また、人間が読みやすい形式でデータを保存できることも特に重要な利点の一つです。

## 作り方

Rubyでテキストファイルを作る方法はいくつかありますが、ここでは`File`クラスを使用する方法を紹介します。まずは以下のように`File`クラスの`open`メソッドを使用してファイルを作成します。

```Ruby
file = File.open("my_file.txt", "w")
```

上の例では`my_file.txt`という名前のファイルを作成しています。そして、次のように`write`メソッドを使ってファイルに書き込む内容を指定します。

```Ruby
file.write("This is a text file written using Ruby.")
```

最後に、ファイルを閉じるために`close`メソッドを使用します。

```Ruby
file.close
```

これで`my_file.txt`に指定した内容が書き込まれます。

## ディープダイブ

テキストファイルを書く際には、ファイルの文字エンコーディングや改行コードにも注意が必要です。また、ファイルを開く際にはエラーが発生する可能性があるため、エラー処理も適切に行う必要があります。さらに、ファイルの読み書きを行う方法やオプションについても学ぶことができます。

## もっと詳しく知るには

- [Rubyによるファイルの読み書き方法](https://qiita.com/sukeirom/items/a9835d2e5b6540eba90e)
- [組み込みクラスFile - Rubyリファレンスマニュアル](https://docs.ruby-lang.org/ja/latest/class/File.html)
- [Ruby 2.6.2 マニュアル - File](https://docs.ruby-lang.org/ja/latest/class/File.html)
- [Rubyでファイル入出力を楽しもう](https://qiita.com/kiyokiyo_kzsby/items/512f5d87f41f45d16d7f)

## 関連記事

- [Rubyとは？初心者にわかりやすく解説！](https://www.sejuku.net/blog/75942)
- [Rubyの基礎とよく使われる文法を理解する](https://www.ruby-lang.org/ja/documentation/ruby-from-other-languages/basic-syntax/)
- [Rubyでプログラミングを学ぶならこれを読め！](https://www.1x1connect.com/column/study/ruby/index.html)