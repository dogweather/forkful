---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:09.891689-07:00
description: "\u65B9\u6CD5\uFF1A Ruby\u306F\u30D5\u30A1\u30A4\u30EB\u64CD\u4F5C\u3092\
  \u30B7\u30F3\u30D7\u30EB\u306B\u3057\u307E\u3059\u3002\u30D5\u30A1\u30A4\u30EB\u306B\
  \u66F8\u304D\u8FBC\u3080\u306B\u306F\u3001Ruby\u306E\u7D44\u307F\u8FBC\u307F`File`\u30AF\
  \u30E9\u30B9\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002\u6B21\u306E\u4F8B\u306F\
  \u3001\u66F8\u304D\u8FBC\u307F\u7528\uFF08`\"w\"`\u30E2\u30FC\u30C9\uFF09\u3068\u8FFD\
  \u8A18\u7528\uFF08`\"a\"`\u30E2\u30FC\u30C9\uFF09\u3067\u30D5\u30A1\u30A4\u30EB\u3092\
  \u958B\u304D\u3001\u6587\u5B57\u5217\u3092\u66F8\u304D\u8FBC\u307F\u3001\u305D\u306E\
  \u5F8C\u30D5\u30A1\u30A4\u30EB\u304C\u9589\u3058\u3089\u308C\u308B\u3053\u3068\u3092\
  \u4FDD\u8A3C\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u3066\u3044\u307E\u3059\uFF1A\
  ."
lastmod: '2024-04-05T22:38:42.363946-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Ruby\u306F\u30D5\u30A1\u30A4\u30EB\u64CD\u4F5C\u3092\u30B7\
  \u30F3\u30D7\u30EB\u306B\u3057\u307E\u3059\u3002\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\
  \u304D\u8FBC\u3080\u306B\u306F\u3001Ruby\u306E\u7D44\u307F\u8FBC\u307F`File`\u30AF\
  \u30E9\u30B9\u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002\u6B21\u306E\u4F8B\u306F\
  \u3001\u66F8\u304D\u8FBC\u307F\u7528\uFF08`\"w\"`\u30E2\u30FC\u30C9\uFF09\u3068\u8FFD\
  \u8A18\u7528\uFF08`\"a\"`\u30E2\u30FC\u30C9\uFF09\u3067\u30D5\u30A1\u30A4\u30EB\u3092\
  \u958B\u304D\u3001\u6587\u5B57\u5217\u3092\u66F8\u304D\u8FBC\u307F\u3001\u305D\u306E\
  \u5F8C\u30D5\u30A1\u30A4\u30EB\u304C\u9589\u3058\u3089\u308C\u308B\u3053\u3068\u3092\
  \u4FDD\u8A3C\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u3066\u3044\u307E\u3059\uFF1A\
  ."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## 方法：
Rubyはファイル操作をシンプルにします。ファイルに書き込むには、Rubyの組み込み`File`クラスを使用できます。次の例は、書き込み用（`"w"`モード）と追記用（`"a"`モード）でファイルを開き、文字列を書き込み、その後ファイルが閉じられることを保証する方法を示しています：

```ruby
# 既存の内容を上書きしてファイルに新しい内容を書き込む
File.open("example.txt", "w") do |file|
  file.puts "Hello, Ruby!"
end

# ファイルの末尾に内容を追加する
File.open("example.txt", "a") do |file|
  file.puts "Adding another line."
end
```
両方のスニペットを実行した後、`example.txt`の内容は以下のようになります：
```
Hello, Ruby!
Adding another line.
```

### サードパーティのライブラリを使う：FileUtils
より複雑なファイル操作には、Ruby標準ライブラリの`FileUtils`が役立ちますが、基本的なファイル書き込みには、標準の`File`メソッドで十分です。ただし、ファイルの書き込みと同時にコピー、移動、削除、またはその他のファイルシステム操作を行いたい場合は、`FileUtils`が役立ちます。

ディレクトリを作成して、そのディレクトリ内のファイルに書き込むために`FileUtils`を使う例：
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/today.log", "w") do |file|
  file.puts "Log entry: #{Time.now}"
end
```

これは、まだ存在しない場合に新しいディレクトリ`logs`を作成し、その中に新しいファイル`today.log`に書き込むことを示しています。これは、FileUtilsで直接書き込むことなく、ディレクトリとファイルの操作を示し、ディレクトリ処理機能を活用しています。
