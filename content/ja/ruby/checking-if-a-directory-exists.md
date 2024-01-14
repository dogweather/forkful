---
title:    "Ruby: ディレクトリが存在するかどうかを確認する"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかを確認する必要がある理由は、プログラミングにおいてよくある作業のひとつです。あるディレクトリが存在するかどうかによって、プログラムの実行結果が変わることがあるため、ディレクトリの存在を確認することは重要です。

## 方法

ディレクトリの存在を確認する方法は非常に簡単です。Rubyプログラミング言語では、`File.exist?`メソッドを使用します。このメソッドは引数として与えられたディレクトリが存在する場合は`true`、存在しない場合は`false`を返します。

```Ruby
directory = "/home/user/documents"
if File.exist?(directory)
  puts "#{directory}は存在します。"
else
  puts "#{directory}は存在しません。"
end
```

上記のコードでは、変数`directory`に存在すると仮定したディレクトリのパスを指定し、`File.exist?`メソッドを使用してそのディレクトリの存在を確認しています。結果は`/home/user/documentsは存在します。`と表示されるでしょう。

## ディープダイブ

`File.exist?`メソッドは内部的には`File::Stat`クラスの`exist?`メソッドを呼び出しています。この`File::Stat`クラスはファイルやディレクトリの情報を保持するオブジェクトです。そして`exist?`メソッドは指定されたファイルやディレクトリが存在するかどうかを判定します。

また、`File.exist?`メソッドはシンボルや文字列、パス名を引数として受け取ることができます。つまり、ディレクトリのパスを文字列で直接指定するだけでなく、変数やメソッドの戻り値を使用してディレクトリの存在を確認することができます。

## 参考リンク

[File.exist?メソッドのドキュメント(Rubyドキュメント)](https://docs.ruby-lang.org/ja/latest/method/File/s/exists%3F.html)  
[File::Statクラスのドキュメント(Rubyドキュメント)](https://docs.ruby-lang.org/ja/latest/class/File=3a=3aStat.html)