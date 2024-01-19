---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Go: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリが存在するかどうかを確認するとは、指定したパスにディレクトリが存在するかどうかを調べるプログラムの方式です。これを行う理由は、対象ディレクトリにアクセスしようとしてエラーを引き起こす前に、予期せぬエラーを避けるためです。

## 方法:

以下は「Dir.exist?」メソッドを使った例です。

```Ruby
# Check if a directory exists
if Dir.exist?('/path/to/directory')
  puts 'Directory exists!'
else
  puts 'Directory does not exist.'
end
```

これが出力されます:

    Directory does not exist.

これは「/path/to/directory」というディレクトリが存在しないことを示しています。

## ディープダイブ

ディレクトリ存在確認の概念は、GUIがない早期のコンピュータで開始され、ディレクトリが存在するかどうかを手動で確認しなければならなかったからです。代替手段として、「File.directory?」を使用することで、指定したパスがディレクトリかどうかを確認することも可能です。しかし、「Dir.exist?」メソッドの方が、ディレクトリの存在だけをチェックするため、推奨されています。

## 参考情報

Ruby公式ドキュメンテーション: [Dir.exist?](https://ruby-doc.org/core-2.6.3/Dir.html#method-c-exist-3F)、[File.directory?](https://ruby-doc.org/core-2.6.3/File.html#method-c-directory-3F)  
プログラミングについて詳しく知りたい方は、[The Pragmatic Bookshelf](https://pragprog.com/)をご覧ください。