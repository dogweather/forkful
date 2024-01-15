---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "Ruby: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかをチェックする理由は、プログラムの実行中に必要なファイルが存在するかどうかを確認するためです。これにより、プログラムが正しく動作するかどうかを事前に確認することができます。

## 方法

例えば、あるディレクトリが存在するかどうかをチェックするには、以下のようにコードを書きます。

```Ruby
if Dir.exist?('sample_directory')
  puts "ディレクトリは存在します。"
else
  puts "ディレクトリは存在しません。"
end
```

上記のように、Dir.exist?メソッドを使用してディレクトリの存在をチェックすることができます。もしディレクトリが存在すれば、真の値を返し、存在しなければ偽の値を返します。そのため、if文を使用して条件分岐を行うことができます。

## 深堀り

ディレクトリの存在をチェックするために使用されるメソッドには「Dir.exist?」の他にも、「Dir.exists?」があります。どちらも同じようにディレクトリの存在をチェックすることができますが、メソッド名に微妙な違いがあります。

具体的には、「Dir.exists?」は「Dir.exist?」の別名で、どちらも同じように動作します。しかし、最新のバージョンのRubyでは「Dir.exists?」は非推奨とされているため、「Dir.exist?」を使用することが推奨されています。

## 参考リンク

- [Ruby 公式ドキュメント](https://docs.ruby-lang.org/ja/latest/class/Dir.html#S_M_DIR_exist)
- [Ruby API ドキュメント](https://docs.ruby-api.com/std-2.7.2/classes/Dir.html#M000005)
- [Ruby Style Guide](https://rubystyle.guide/#predicate-question-methods)