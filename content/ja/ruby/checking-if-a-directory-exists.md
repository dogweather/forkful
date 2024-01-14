---
title:    "Ruby: ディレクトリが存在するかどうかを調べる"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかを確認することに取り組んだ方が良い理由は、コードを実行する前に予期しないエラーを防ぐことができるからです。プログラム内で特定のディレクトリにアクセスする必要がある場合、そのディレクトリが存在するかどうかを事前に確認することが重要です。

## 方法

Rubyでは、ディレクトリが存在するかを確認するために`Dir.exist?`メソッドを使用します。例えば、`check_dir.rb`というファイル内で以下のコードを使用することで、`examples`というディレクトリが存在するかを確認することができます。

```Ruby
if Dir.exist?("examples")
  puts "Directory exists"
else
  puts "Directory does not exist"
end
```

もしディレクトリが存在する場合、`Directory exists`という出力が表示されます。しかし、もしディレクトリが存在しない場合、`Directory does not exist`という出力が表示されます。

## 深堀り

`Dir.exist?`メソッドは、ディレクトリが存在するかどうかを確認するだけではなく、指定したディレクトリ内にあるすべてのファイルやサブディレクトリも含めて確認することができます。さらに、`Dir.exist?`メソッドはディレクトリのパスを渡すこともでき、相対パスや絶対パスの両方で動作します。

## 参考

- [RubyのDirクラスドキュメント](https://docs.ruby-lang.org/en/2.7.0/Dir.html)
- [Rubyの存在確認メソッド「Dir.exist?」を理解しよう](https://qiita.com/t0rkie/items/92ec4f6fc388b8dbd922)