---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# テンポラリファイルの作成 - Ruby スタイル 

## 何となぜ？
テンポラリファイルとは、一時的に作られ、使用後に消去されるファイルです。これは、短時間で多量のデータを処理するため、またプログラム間でデータを交換するためにプログラマーが作成します。

## どうやる？
Rubyでは`Tempfile`クラスを使って簡単にテンポラリファイルを作成します。以下はその例です:

```Ruby
require 'tempfile'

temp_file = Tempfile.new('tempfile')
temp_file.write('Hello, Ruby!')
temp_file.rewind
puts temp_file.read # "Hello, Ruby!"
temp_file.close
temp_file.unlink # ファイルを削除
```

## ディープダイブ
1. **歴史的な背景**: Ruby 1.8以降、`Tempfile`クラスが組み込みライブラリとして提供され、簡単にテンポラリファイルを扱うことができます。
2. **代替案**: `Tempfile`以外にも、`File`クラスの`#write`や`#read`メソッドで一時的なデータ保存を行うことも可能です。
3. **実装詳細**: `Tempfile`クラスは、自動的に一意な名前を生成し、プログラム終了時にファイルを削除します。ファイルを明示的に削除する場合は、`#unlink`または`#delete`メソッドを使用します。

## 参考情報
* [Tempfile (Ruby 2.7.0 リファレンスマニュアル)](https://docs.ruby-lang.org/ja/latest/class/Tempfile.html)
* [File (Ruby 2.7.0 リファレンスマニュアル)](https://docs.ruby-lang.org/ja/latest/class/File.html)
* [Rubyで一時ファイル(Tempfile)を使う](https://www.javadrive.jp/ruby/ior/index5.html)