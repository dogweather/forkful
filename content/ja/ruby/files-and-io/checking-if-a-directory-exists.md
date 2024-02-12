---
title:                "ディレクトリが存在するかどうかの確認"
aliases:
- /ja/ruby/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:22.285031-07:00
model:                 gpt-4-0125-preview
simple_title:         "ディレクトリが存在するかどうかの確認"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
Rubyでディレクトリが存在するかを確認することは、プログラマがファイルの読み取りや新しいディレクトリの作成のような操作を実行する前にディレクトリの存在を検証することを可能にします。これは、ファイル処理のエラーを避け、ファイルシステムの操作の信頼性を保証するために重要です。

## 方法：
Rubyの標準ライブラリは、ディレクトリの存在をチェックするための直接的な方法を提供します。こちらは純粋なRubyだけを使ってそれを行う方法です、何のサードパーティライブラリも必要ありません：

```ruby
require 'fileutils'

# ディレクトリが存在するか確認
if Dir.exist?('/path/to/directory')
  puts 'ディレクトリが存在します。'
else
  puts 'ディレクトリは存在しません。'
end
```
サンプル出力：
```
ディレクトリが存在します。
```
または：
```
ディレクトリは存在しません。
```

`Dir.exist?`を使う以外にも、与えられたパスがディレクトリである場合`true`を返す`File.directory?`メソッドを利用することもできます：

```ruby
if File.directory?('/path/to/directory')
  puts 'ディレクトリが存在します。'
else
  puts 'ディレクトリは存在しません。'
end
```
`Dir.exist?`と`File.directory?`の両方はRubyの標準ライブラリの一部であり、使用するために外部のgemsを必要とせず、ディレクトリのチェックに便利かつ効率的なオプションとなっています。
