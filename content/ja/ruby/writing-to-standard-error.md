---
title:                "Ruby: 標準エラーへの書き込み"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

プログラミングをする際、エラーが発生することはよくあります。このエラーを解決するためには、その原因を特定する必要があります。そのためには、標準エラーへの書き込みが非常に重要です。

## How To

あなたがRubyでプログラミングをしている場合、標準エラーへの書き込みはとても簡単です。次の例をご覧ください。

```Ruby
puts "This will be output to standard output!"

$stderr.puts "This will be written to standard error!"
```

上記のコードを実行すると、標準出力には"This will be output to standard output!"という文が表示されますが、標準エラーには"This will be written to standard error!"という文が書き込まれます。

このように、単純にputsコマンドをstderr.putsコマンドに変えるだけで、エラーが発生した場合にその原因を特定することができます。

## Deep Dive

標準エラーへの書き込みは、プログラミングにおいて非常に重要です。なぜなら、標準出力と標準エラーは別々のストリームであり、標準エラーに書き込まれたエラーメッセージは標準出力に表示されません。そのため、プログラムを実行した際にエラーが出力された場合にも、正常な出力結果を確認することができます。

また、標準エラーには様々な情報を書き込むことができます。個人情報を含むようなデータを標準エラーに書き込むことで、標準出力には表示されないようなセキュリティ上の情報を保護することができます。

## See Also

- [Official Ruby Documentation on stderr](https://ruby-doc.org/core-2.7.1/IO.html#method-c-stderr)
- [Difference between STDERR and STDOUT in Ruby](https://stackoverflow.com/questions/664514/what-is-the-difference-between-stderr-and-stdout-in-ruby)
- [Understanding Standard Streams in Ruby](https://www.rubyguides.com/2016/05/ruby-io/)