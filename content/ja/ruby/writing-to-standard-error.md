---
title:                "標準エラーへの書き込み"
aliases:
- ja/ruby/writing-to-standard-error.md
date:                  2024-02-03T19:34:38.139058-07:00
model:                 gpt-4-0125-preview
simple_title:         "標準エラーへの書き込み"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
Rubyでの標準エラー（stderr）への書き込みは、エラーメッセージや診断情報を標準出力（stdout）とは異なる別の出力ストリームへ向けることについてです。プログラマーは、通常のプログラム出力とエラーやデバッグ情報を区別するためにこれを行います。これにより、問題の診断やログの解析が容易になります。

## 方法：
Rubyの標準ライブラリは、`$stderr`や`STDERR`を使用してstderrへ簡単に書き込む方法を提供します。この基本的な操作にはサードパーティのライブラリは必要ありません。

### stderrへのシンプルなメッセージの書き込み：
```ruby
$stderr.puts "Error: File not found."
# または同等に
STDERR.puts "Error: File not found."
```
サンプル出力（stderrへ）：
```
Error: File not found.
```

### stderrをファイルにリダイレクトする：
```ruby
File.open('error.log', 'w') do |file|
  STDERR.reopen(file)
  STDERR.puts "Failed to open configuration."
end
```
このコードスニペットは、stderrを`error.log`という名前のファイルにリダイレクトし、プログラムがstderrのリダイレクションをリセットするか終了するまで、すべての後続の書き込まれたエラーがそこに出力されます。

### 例外処理としてのstderrの使用：
```ruby
begin
  # 失敗する可能性のある操作をシミュレートする、例えば、ファイルを開く
  File.open('nonexistent_file.txt')
rescue Exception => e
  STDERR.puts "Exception occurred: #{e.message}"
end
```
サンプル出力（stderrへ）：
```
Exception occurred: No such file or directory @ rb_sysopen - nonexistent_file.txt
```

Rubyの組み込みメソッドでstderrへの書き込みは多くのアプリケーションにとって十分ですが、より複雑なロギングのニーズについては、`logger`標準ライブラリや`Log4r`のような外部のgemを検討すると良いでしょう。これらは、重大度レベル、書式設定、ファイル、メールなどのさまざまな出力に書き込む能力を含む、設定可能なロギングメカニズムを提供します。
