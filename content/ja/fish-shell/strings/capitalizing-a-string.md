---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:53.465910-07:00
description: "\u65B9\u6CD5: Fish Shell\u3067\u306F\u3001\u5916\u90E8\u30C4\u30FC\u30EB\
  \u3084\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5FC5\u8981\u3068\u305B\u305A\u306B\u3001\
  \u7D44\u307F\u8FBC\u307F\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u6587\u5B57\u5217\
  \u3092\u76F4\u63A5\u64CD\u4F5C\u3067\u304D\u307E\u3059\u3002\u6587\u5B57\u5217\u3092\
  \u5927\u6587\u5B57\u5316\u3059\u308B\u306B\u306F\u3001`string`\u30B3\u30DE\u30F3\
  \u30C9\u3068\u30B5\u30D6\u30B3\u30DE\u30F3\u30C9\u3092\u7D44\u307F\u5408\u308F\u305B\
  \u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:43.496470-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell\u3067\u306F\u3001\u5916\u90E8\u30C4\u30FC\u30EB\u3084\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u3092\u5FC5\u8981\u3068\u305B\u305A\u306B\u3001\u7D44\u307F\u8FBC\
  \u307F\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u6587\u5B57\u5217\u3092\u76F4\u63A5\
  \u64CD\u4F5C\u3067\u304D\u307E\u3059\u3002\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\
  \u5316\u3059\u308B\u306B\u306F\u3001`string`\u30B3\u30DE\u30F3\u30C9\u3068\u30B5\
  \u30D6\u30B3\u30DE\u30F3\u30C9\u3092\u7D44\u307F\u5408\u308F\u305B\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 方法:
Fish Shellでは、外部ツールやライブラリを必要とせずに、組み込み関数を使用して文字列を直接操作できます。文字列を大文字化するには、`string`コマンドとサブコマンドを組み合わせます。

```fish
# サンプル文字列
set sample_string "hello world"

# 最初の文字を大文字に
set capitalized_string (string sub -l 1 -- $sample_string | string upper)(string sub -s 2 -- $sample_string)

echo $capitalized_string
```

出力:
```
Hello world
```

文字列内の複数の単語を大文字化する必要があるシナリオ（例: "hello world" を "Hello World" に変換する）では、それぞれの単語に対して大文字化のロジックを適用しながら繰り返します：

```fish
# サンプル文
set sentence "hello fish shell programming"

# 各単語を大文字に
set capitalized_words (string split " " -- $sentence | while read -l word; string sub -l 1 -- $word | string upper; and string sub -s 2 -- $word; end)

# 大文字化された単語を結合
set capitalized_sentence (string join " " -- $capitalized_words)

echo $capitalized_sentence
```

出力:
```
Hello Fish Shell Programming
```

Fish Shellは、いくつかのプログラミング言語がその文字列メソッドで提供するような、完全な文の大文字化を一つのコマンドで直接行う機能を提供していません。したがって、`string split`、`string sub`、`string upper`を組み合わせ、再度結合する方法は、この目的を達成するためのFish Shellにおける慣用的なアプローチを表します。
