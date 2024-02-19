---
aliases:
- /ja/ruby/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:09.891689-07:00
description: "Ruby\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\
  \u304D\u8FBC\u3080\u3053\u3068\u306F\u3001\u51FA\u529B\u3084\u30C7\u30FC\u30BF\u3092\
  \u6C38\u7D9A\u7684\u306B\u4FDD\u5B58\u3059\u308B\u57FA\u672C\u7684\u306A\u64CD\u4F5C\
  \u3067\u3042\u308A\u3001\u5F8C\u3067\u30C7\u30FC\u30BF\u306B\u30A2\u30AF\u30BB\u30B9\
  \u3057\u305F\u308A\u5909\u66F4\u3057\u305F\u308A\u3059\u308B\u3053\u3068\u3092\u53EF\
  \u80FD\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30ED\u30B0\u8A18\u9332\u3001\u8A2D\u5B9A\u306E\u4FDD\u5B58\u3001\u307E\u305F\u306F\
  \u4EBA\u9593\u304C\u8AAD\u3081\u308B\u5F62\u5F0F\u3067\u30C7\u30FC\u30BF\u3092\u30A8\
  \u30AF\u30B9\u30DD\u30FC\u30C8\u3059\u308B\u306A\u3069\u306E\u7406\u7531\u3067\u3001\
  \u3053\u306E\u30BF\u30B9\u30AF\u3092\u983B\u7E41\u306B\u5B9F\u884C\u3057\u307E\u3059\
  \u3002"
lastmod: 2024-02-18 23:08:55.408063
model: gpt-4-0125-preview
summary: "Ruby\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\u304D\
  \u8FBC\u3080\u3053\u3068\u306F\u3001\u51FA\u529B\u3084\u30C7\u30FC\u30BF\u3092\u6C38\
  \u7D9A\u7684\u306B\u4FDD\u5B58\u3059\u308B\u57FA\u672C\u7684\u306A\u64CD\u4F5C\u3067\
  \u3042\u308A\u3001\u5F8C\u3067\u30C7\u30FC\u30BF\u306B\u30A2\u30AF\u30BB\u30B9\u3057\
  \u305F\u308A\u5909\u66F4\u3057\u305F\u308A\u3059\u308B\u3053\u3068\u3092\u53EF\u80FD\
  \u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30ED\
  \u30B0\u8A18\u9332\u3001\u8A2D\u5B9A\u306E\u4FDD\u5B58\u3001\u307E\u305F\u306F\u4EBA\
  \u9593\u304C\u8AAD\u3081\u308B\u5F62\u5F0F\u3067\u30C7\u30FC\u30BF\u3092\u30A8\u30AF\
  \u30B9\u30DD\u30FC\u30C8\u3059\u308B\u306A\u3069\u306E\u7406\u7531\u3067\u3001\u3053\
  \u306E\u30BF\u30B9\u30AF\u3092\u983B\u7E41\u306B\u5B9F\u884C\u3057\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？
Rubyでテキストファイルに書き込むことは、出力やデータを永続的に保存する基本的な操作であり、後でデータにアクセスしたり変更したりすることを可能にします。プログラマーは、ログ記録、設定の保存、または人間が読める形式でデータをエクスポートするなどの理由で、このタスクを頻繁に実行します。

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
