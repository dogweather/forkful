---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
simple_title:         "テキストファイルの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?（何となぜ？）
テキストファイルへの書き込みは、データを永続化するプロセスです。プログラマーは、設定、データ交換、またはログ情報を保存するためにこれを行います。

## How to:（方法）
Rubyでテキストファイルに書き込む基本的な方法を見てみましょう。

```ruby
# ファイルを新規作成して書き込む
File.write('example.txt', "Hello, Ruby world!")

# ファイルを開いてから書き込む
File.open('example.txt', 'w') do |file|
  file.puts "もう一行追加"
end
```

ファイル`example.txt`の中身:
```
Hello, Ruby world!
もう一行追加
```

## Deep Dive（深掘り）
Rubyは最初からファイルIOに対応していて、シンプルな`File.write`メソッドから`IO`クラスを通じた複雑な操作まで多岐にわたります。`write`メソッドは実際には`IO.write`から来ており、ショートカットとして便利です。`File.open`を利用すると、ブロック内でファイルを安全に書き込んだ後に自動的にクローズが行われます。他の言語と異なり、Rubyではシンプルなシンタックスでこれを行えるため、コードが読みやすくなります。

## See Also（関連情報）
- [RubyのIOクラス](https://docs.ruby-lang.org/ja/latest/class/IO.html)
- [RubyのFileクラス](https://docs.ruby-lang.org/ja/latest/class/File.html)
- [Ruby-Doc.org](https://www.ruby-doc.org/core-3.1.2/File.html)
