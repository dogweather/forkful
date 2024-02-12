---
title:                "テキストファイルの作成"
aliases: - /ja/ruby/writing-a-text-file.md
date:                  2024-02-03T19:29:09.891689-07:00
model:                 gpt-4-0125-preview
simple_title:         "テキストファイルの作成"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
