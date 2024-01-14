---
title:                "Ruby: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

プログラマーとして、一時ファイルを作成することは非常に便利です。一時ファイルを使用することで、プログラムの実行中に一時的にデータを保持し、より効率的にコードを書くことができます。

## 作り方

Rubyでは、`Tempfile`クラスを使用して一時ファイルを作成することができます。以下のコード例を参考にしてください。

```ruby
require "tempfile"

# 一時ファイルの作成
tmpfile = Tempfile.new("my_temp_file")

# ファイルにデータを書き込む
tmpfile.write("これは一時ファイルに書き込まれたデータです。")

# ファイルを閉じる
tmpfile.close
```
上記のコードでは、`"my_temp_file"`という名前で一時ファイルを作成し、その中に文字列を書き込んでいます。また、必要な時には`tempfile.close`を使用して一時ファイルを閉じることができます。

一時ファイルを作成する際には、ファイルが削除されるタイミングや作成される場所にも注意が必要です。詳細な内容については、次のセクションで解説します。

## 詳細について

一時ファイルを作成する際、注意しなければならないポイントが幾つかあります。

まず、一時ファイルはプログラムが実行中にディスク上に作成されますが、その後必要な時に自動的に削除されます。このタイミングは、プラットフォームごとに異なります。また、ファイルが削除される前にプログラムが終了してしまった場合、一時ファイルは残ったままになります。そのため、必要に応じてファイルを削除する処理を明示的に記述する必要があります。

次に、`Tempfile`クラスには第二引数として`Tempfile.new("prefix", "tmpdir")`という指定も可能です。この場合、指定した文字列を接頭語として、指定したディレクトリ内に一時ファイルが作成されます。特に指定しない場合、デフォルトの一時ディレクトリが使用されます。

## 関連情報

- [Rubyドキュメント: Tempfileクラス](https://docs.ruby-lang.org/ja/latest/class/Tempfile.html)
- [Rubyガイド：一時ファイルの作成と削除](https://rubycoding.net/一時ファイルの作成と削除/)

## その他参考リンク

- [RubyGuides: Temporary Files in Ruby](https://www.rubyguides.com/2015/04/working-with-temporary-files-in-ruby/)
- [TechNote: Creating and Handling Temporary Files in Ruby](https://technotes.iangreenleaf.com/posts/creating-and-handling-temporary-files-in-ruby/)