---
title:                "標準エラーへの書き込み"
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？

標準エラーへの書き込みは、プログラムが遭遇する問題や警告を報告するためです。これによりログファイルや標準出力とエラーメッセージを区別でき、デバッグがしやすくなります。

## How to: / 方法

Rubyで標準エラーに書き込むには、`$stderr`や`STDERR`を使います。

```Ruby
# 標準出力への書き込み
puts "これは標準出力です。"

# 標準エラーへの書き込み
$stderr.puts "これは標準エラーです。"
# または
STDERR.puts "これは標準エラーです。"
```

実行すると、以下の出力が得られます。

```
これは標準出力です。
これは標準エラーです。
```

標準出力と標準エラーの出力先はデフォルトでは同じですが、リダイレクトして分けることができます。

## Deep Dive / 詳細情報

標準エラーはUNIX系システムで長い歴史を持ちます。プログラムは標準出力(stdout)と標準エラー(stderr)を使用して、それぞれ異なる情報を出力できます。

もし`puts`や`print`メソッドを使う代わりに低レベルの操作が必要な場合は、`write`メソッドを使います。
```Ruby
STDERR.write("エラーメッセージ\n")
```

レスキューブロック内で例外情報を標準エラーに書き込む例:
```Ruby
begin
  # 危険な操作
rescue => e
  STDERR.puts "エラーが発生しました: #{e.message}"
end
```

標準エラーに書き込むのは、主にデバッグやエラーログを記録する場合に利用されます。しかし、実行時に重要な通知や警告をユーザーに伝達する場面でも使用されます。

## See Also / 関連情報

- さらに詳しいUNIXの標準ストリーム: [Standard Streams - Wikipedia](https://en.wikipedia.org/wiki/Standard_streams)