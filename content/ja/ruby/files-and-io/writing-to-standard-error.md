---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:38.139058-07:00
description: "\u65B9\u6CD5\uFF1A Ruby\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u306F\u3001`$stderr`\u3084`STDERR`\u3092\u4F7F\u7528\u3057\u3066stderr\u3078\u7C21\
  \u5358\u306B\u66F8\u304D\u8FBC\u3080\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\
  \u3002\u3053\u306E\u57FA\u672C\u7684\u306A\u64CD\u4F5C\u306B\u306F\u30B5\u30FC\u30C9\
  \u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u5FC5\u8981\u3042\
  \u308A\u307E\u305B\u3093\u3002"
lastmod: '2024-03-13T22:44:42.877495-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u3001`$stderr`\u3084\
  `STDERR`\u3092\u4F7F\u7528\u3057\u3066stderr\u3078\u7C21\u5358\u306B\u66F8\u304D\
  \u8FBC\u3080\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u3053\u306E\u57FA\
  \u672C\u7684\u306A\u64CD\u4F5C\u306B\u306F\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\
  \u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u5FC5\u8981\u3042\u308A\u307E\u305B\u3093\
  ."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

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
