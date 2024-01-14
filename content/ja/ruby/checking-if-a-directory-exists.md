---
title:                "Ruby: ディレクトリが存在するかどうかをチェックする"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかをチェックする理由は、ファイルを作成したり、削除したり、または特定のディレクトリ内のファイルにアクセスする必要があるためです。

## 方法

まず、Rubyプログラミング言語でディレクトリが存在するかどうかを確認する方法について説明します。次のコードブロックを使用します。

```Ruby
# ディレクトリが存在するかどうかをチェックする
Dir.exist?('/path/to/directory')
```

これにより、指定したディレクトリが存在する場合は`true`、存在しない場合は`false`が返されます。また、簡単に条件分岐を使用して、ディレクトリが存在するかどうかに応じて処理を分岐させることもできます。

```Ruby
if Dir.exist?('/path/to/directory')
  # ディレクトリが存在する場合の処理
else
  # ディレクトリが存在しない場合の処理
end
```

必要に応じて、指定したディレクトリの配下に存在するファイルやディレクトリの一覧を取得することもできます。

```Ruby
# ディレクトリ内のファイルとディレクトリの一覧を取得する
Dir.entries('/path/to/directory')
```

このように、`Dir.exist?`や`Dir.entries`を組み合わせることで、特定のディレクトリ内でのファイル操作や、ファイルの一覧表示を容易に行うことができます。

## ディープダイブ

ディレクトリが存在するかどうかをチェックするメソッド`exist?`は、`Dir`クラスのメソッドの1つであることはご存知かもしれません。しかし、実際にはこのメソッドは`File`クラスでも使用することができます。つまり、`File.exist?`を使用して、ファイルが存在するかどうかをチェックすることもできるのです。

さらに、`Dir.exist?`や`File.exist?`以外にも、`Dir`クラスや`File`クラスにはディレクトリやファイルに関する様々なメソッドが用意されています。ぜひ、公式ドキュメントを参考に、さまざまなメソッドを使いこなしてみてください。

## 詳しくはこちらを参照

[Dirクラスの公式ドキュメント](https://ruby-doc.org/core-3.0.0/Dir.html)

[Fileクラスの公式ドキュメント](https://ruby-doc.org/core-3.0.0/File.html#method-c-exist-3F)

[ドットインストールの「Ruby入門」動画：ディレクトリ操作編](https://dotinstall.com/lessons/basic_ruby_v3/28405)