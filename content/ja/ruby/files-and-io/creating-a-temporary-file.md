---
date: 2024-01-20 17:41:10.336865-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:38:42.365052-06:00'
model: gpt-4-1106-preview
summary: ":mktmpdir`\u3067\u4E00\u6642\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u3092\u4F5C\
  \u6210\u3059\u308B\u65B9\u6CD5\u306A\u3069\u304C\u3042\u308A\u307E\u3059\u3002`Tempfile`\u306F\
  \u5185\u90E8\u7684\u306B\u306F`File`\u30AF\u30E9\u30B9\u3068`Dir::Tmpname`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u3063\u3066\u304A\u308A\u3001\u540D\u524D\u885D\
  \u7A81\u3092\u907F\u3051\u306A\u304C\u3089\u30BB\u30AD\u30E5\u30A2\u306B\u30D5\u30A1\
  \u30A4\u30EB\u3092\u4F5C\u6210\u3057\u307E\u3059\u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

## How to (やり方)
```ruby
require 'tempfile'

# 一時ファイルを作成
temp_file = Tempfile.new('my_temp')

# ファイルにデータを書き込み
temp_file.write('Hello Ruby!')

# ファイルを閉じる前に、必要な操作をする
temp_file.rewind
puts temp_file.read  #=> "Hello Ruby!"

# ファイルを閉じて、自動的に削除する
temp_file.close
temp_file.unlink
```

## Deep Dive (詳細な情報)
一時ファイルの概念は古く、オペレーティングシステムがファイルシステムを使い始めた時からあります。選択肢としては、Rubyの標準ライブラリの`Tempfile`だけでなく、`File#open`にブロックを渡す方法や、`Dir::mktmpdir`で一時ディレクトリを作成する方法などがあります。`Tempfile`は内部的には`File`クラスと`Dir::Tmpname`モジュールを使っており、名前衝突を避けながらセキュアにファイルを作成します。

## See Also (関連項目)
- [Rubyのドキュメント: File](https://ruby-doc.org/core/File.html)
