---
title:                "Ruby: 標準エラーに書き込む"
simple_title:         "標準エラーに書き込む"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ
Rubyプログラミングをしている人は、時には標準エラーに書き込む必要があります。これはデバッグ中にエラーメッセージを表示するためや、プログラムの実行中に重要な情報を表示するためです。

## 方法
以下のコードブロック内にある、Rubyの標準エラーに書き込む方法をご紹介します。

```
Rubyのコード例
標準エラーに書き込むコード
```

実行すると、標準エラーに指定したメッセージが表示されます。この方法を使えば、プログラムのデバッグがより簡単になります。

## 深堀り
標準エラーに書き込む方法は、標準出力に書き込む方法と似ています。ただし、標準出力の場合には`puts`メソッドを使いますが、標準エラーの場合には`$stderr.puts`メソッドを使います。また、エラーメッセージを表示する際には、文字列に赤色のテキストを加えることでより目立たせることができます。

例えば、以下のコードを実行すると、`backtrace`メソッドによるエラーメッセージが赤色で表示されます。

```
Rubyのコード例
begin
  # 何らかのエラーが発生した場合
  raise StandardError, "エラーメッセージ"
rescue StandardError => e
  $stderr.puts "エラーが発生しました: #{e.backtrace}"
end
```

## 他に参考になるウェブサイト
- [Rubyのドキュメント](https://docs.ruby-lang.org/ja/latest/class/StandardError.html)
- [標準エラーの表示方法についての記事](https://techacademy.jp/magazine/45289)
- [エラーメッセージに色をつける方法についてのブログ記事](https://naoreki.com/blog/2016-08-23-making-colored-ascii-art)


## 関連するリンク
- [Rubyのエラー処理についての記事](https://www.rubylife.jp/exception/)
- [標準出力についてのドキュメント](https://docs.ruby-lang.org/ja/latest/method/$stdout/s/puts.html)