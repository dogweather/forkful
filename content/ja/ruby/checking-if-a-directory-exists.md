---
title:                "Ruby: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認することの重要性は、プログラミングにおいてファイルの操作を行う上で欠かせないものです。例えば、あるファイルを読み込む前に事前にそのディレクトリが存在するかどうかを確認することで、プログラムがエラーを発生させることなく安全に実行することができます。

## 方法

ディレクトリが存在するかどうかを確認するには、Rubyの```File```クラスの```exist?```メソッドを使用します。以下のようにコードを書くことで、任意のディレクトリが存在するかどうかを確認することができます。

```Ruby
if File.exist?("/path/to/directory")
  puts "ディレクトリが存在します"
else
  puts "ディレクトリが存在しません"
end
```

上記のコードでは、「/path/to/directory」の部分を確認したいディレクトリのパスに変更し、実行することで結果を確認することができます。

## 深堀り

実際にはどのようにして```File```クラスの```exist?```メソッドがディレクトリの存在を確認しているか気になりますよね。実はこのメソッドは、受け取ったパスを元にファイルシステムを操作し、対象のディレクトリが存在するかどうかを確認しています。そのため、正しいパスが与えられた場合は```true```を返し、そうでない場合は```false```を返します。

## 参考リンク

- [RubyのFileクラスドキュメント](https://docs.ruby-lang.org/ja/latest/class/File.html)
- [RubyのIOクラスドキュメント](https://docs.ruby-lang.org/ja/latest/class/IO.html)
- [Rubyでファイル操作を行う方法](https://techacademy.jp/magazine/4155)

## 参考になるリソースの例

上記の記事では、ディレクトリが存在するかどうかを確認する方法について説明しました。ぜひ上記のリンクを参考にして、Rubyでのファイル操作について更に学習を進めてください。