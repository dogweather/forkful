---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

テキストファイルの読み取りは、ファイルから文字情報を取得するプロセスです。プログラマーは、データ分析、設定読み取り、または内容表示などの目的でこれを行います。

## どうするのか：

次のRubyコードは、テキストファイルを読み取ります。

```Ruby
File.open('example.txt', 'r') do |f|
  while line = f.gets
    puts line
  end
end
```

このスクリプトは'example.txt'を開き、各行を読み取り、その内容を出力します。

## 深掘り：

歴史的な文脈では、テキストファイルの読み取りはプログラミングのもっとも基本的なタスクの一つです。選択肢としては、Ruby以外にもPythonやJavaなど、他の多くの言語でこの操作を行うことができます。

ファイルを読み込む際の詳細については、'File.open'メソッドはファイルを開くためのRubyの組み込み関数であり、'r'フラグは読み取り専用モードでファイルを開きます。'gets'メソッドはファイルから次の行を読み取ります。

## 参考情報：

詳細とその他の関連情報については、以下のリンクを参照してください：

[Fileクラス (Ruby 3.0.0 リファレンスマニュアル)](https://docs.ruby-lang.org/ja/latest/class/File.html)

[Rubyでファイル読み書き（入出力）](https://www.atmarkit.co.jp/ait/articles/2001/03/news022.html)

[ファイルの読み込み、書き込みなど（Rubyプログラミング）](https://www.javadrive.jp/ruby/file/index3.html)