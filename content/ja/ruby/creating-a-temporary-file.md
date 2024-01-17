---
title:                "「一時ファイルの作成」"
html_title:           "Ruby: 「一時ファイルの作成」"
simple_title:         "「一時ファイルの作成」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## あのね、そうなんだ、一時ファイルを作るってなんだろう？なぜプログラマーがそれをするの？

## どうやるんだい？
```Ruby
# 一時ファイルを作成する
file = Tempfile.new

# テキストを書き込む
file.write("こんにちは、世界！")

# ファイルを読み込む
file.read
#=> "こんにちは、世界！"

# ファイルを閉じる
file.close

# 一時ファイルの削除
file.unlink
```

## 深く掘り下げると？
一時ファイルとは、一時的にデータを保存するために作成されるファイルのことです。プログラマーは、プログラム内で動的にファイルを作成する必要がある場合に使用します。一時ファイルは、プログラムの実行が終了すると自動的に削除されるので、ディスク容量を節約するためにも重要です。

代替手段としては、プログラム内でデータをメモリ上に保持する方法がありますが、一時ファイルを使用する方がメモリの使用量を減らすことができます。一時ファイルの実装方法については、各プログラミング言語のマニュアルを参照してください。

## もっと詳しく知りたい方へ
- [RubyのTempfileクラスのドキュメント](https://ruby-doc.org/stdlib-2.6.4/libdoc/tempfile/rdoc/Tempfile.html)
- [一時ファイルに関する詳細な解説記事](https://devblog.theoryspace.com/temporary-files-in-ruby/)
- [プログラミングでの一時ファイル作成の活用法](https://medium.com/@jeroenptrs/using-temporary-files-in-ruby-everything-you-need-to-know-22da8871e160)