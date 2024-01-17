---
title:                "「ディレクトリが存在するかどうかを確認する」"
html_title:           "Ruby: 「ディレクトリが存在するかどうかを確認する」"
simple_title:         "「ディレクトリが存在するかどうかを確認する」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

＃＃ 何となぜ？

ディレクトリが存在しているかどうかをチェックすることは、プログラマーが特定のアクションを実行する前に必要なステップです。例えば、ファイルを読み込む前に、そのファイルが実際に存在しているかどうかを確認する必要があります。これにより、不必要なエラーやバグを防ぐことができます。

＃＃ 方法：

```Ruby
if File.directory?("path/to/directory")
  puts "Directory exists!"
else
  puts "Directory does not exist."
end
```

出力：

```
Directory exists!
```

＃＃ 深く掘り下げる

（1）歴史的な文脈：ディレクトリの存在をチェックするメソッドは、長い間コンピューターの世界で使用されてきました。初期のオペレーティングシステムでは、ディレクトリの存在をチェックすることはより手間のかかる作業でした。

（2）代替方法：ディレクトリが存在するかどうかを調べるために使用する方法はいくつかあります。上記のコードの場合、`File.exist?()`メソッドを使用することもできます。

（3）実装の詳細：Rubyでは、特別なメソッドを使用して、ディレクトリの存在をチェックすることができます。これは、ファイルシステムへのアクセスを簡単にするために提供されています。

＃＃ 関連情報を参照

[Fileクラス](https://ruby-doc.org/core-2.7.1/File.html) 、[Dirクラス](https://ruby-doc.org/core-2.7.1/Dir.html)、および[Pathnameクラス](https://ruby-doc.org/stdlib-2.7.1/libdoc/pathname/rdoc/Pathname.html) のドキュメンテーションを参照してください。