---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:38.139058-07:00
description: "Ruby\u3067\u306E\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\
  \u306E\u66F8\u304D\u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\
  \u30B8\u3084\u8A3A\u65AD\u60C5\u5831\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\
  \u3068\u306F\u7570\u306A\u308B\u5225\u306E\u51FA\u529B\u30B9\u30C8\u30EA\u30FC\u30E0\
  \u3078\u5411\u3051\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u901A\u5E38\u306E\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u51FA\u529B\u3068\u30A8\u30E9\u30FC\u3084\u30C7\u30D0\u30C3\u30B0\u60C5\u5831\
  \u3092\u533A\u5225\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\
  \u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u554F\u984C\u306E\u8A3A\u65AD\u3084\
  \u30ED\u30B0\u306E\u89E3\u6790\u304C\u5BB9\u6613\u306B\u306A\u308A\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.877495-06:00'
model: gpt-4-0125-preview
summary: "Ruby\u3067\u306E\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\
  \u66F8\u304D\u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u3084\u8A3A\u65AD\u60C5\u5831\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\u3068\
  \u306F\u7570\u306A\u308B\u5225\u306E\u51FA\u529B\u30B9\u30C8\u30EA\u30FC\u30E0\u3078\
  \u5411\u3051\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u901A\u5E38\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u51FA\u529B\u3068\u30A8\u30E9\u30FC\u3084\u30C7\u30D0\u30C3\u30B0\u60C5\u5831\u3092\
  \u533A\u5225\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002\u3053\u308C\u306B\u3088\u308A\u3001\u554F\u984C\u306E\u8A3A\u65AD\u3084\u30ED\
  \u30B0\u306E\u89E3\u6790\u304C\u5BB9\u6613\u306B\u306A\u308A\u307E\u3059\u3002."
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
