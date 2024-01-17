---
title:                "「標準エラーへの書き込み」"
html_title:           "Ruby: 「標準エラーへの書き込み」"
simple_title:         "「標準エラーへの書き込み」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 何で & なぜ？ 
標準エラー出力とは何か？プログラマーはなぜそれをするのか？

標準エラー出力とは、プログラムの実行中にエラーメッセージを表示するための手段です。これにより、プログラマーは実行中に発生したエラーをすばやく把握することができます。プログラマーは、エラーが発生した原因を追跡し修正するために、重要な情報を提供するために、このアウトプットを使用します。

## 使い方：
```Ruby
# 標準エラー出力のために、$stderrを使用します
$stderr.puts "エラーメッセージ"

# 標準エラー出力を無視するために、/dev/nullを使用します
$stderr.reopen(File::NULL)
```

```Ruby
# 標準入力から値を受け取り、標準エラー出力にエラーメッセージを表示します
input = gets.chomp
if input.empty?
  $stderr.puts "入力が空です"
end
```

## 深堀り：
標準エラー出力は、1970年代のUNIXオペレーティングシステムで作成された標準的な機能です。プログラマーは、デバッグやエラーの追跡が容易になるように、この機能を使用します。標準出力と比較して、標準エラー出力はカラフルなテキストが出力されます。代替手段として、プログラマーはログファイルやメールアラートを使用することもできます。`puts`や`print`などのメソッドは、標準エラー出力ではなく、標準出力にテキストを出力します。

## さらに見る：
詳細は、[Rubyドキュメント](https://docs.ruby-lang.org/ja/latest/doc/index.html)や[スタックオーバーフロー](https://stackoverflow.com/questions/232956/what-is-the-difference-between-stderr-and-stdout)を参照してください。