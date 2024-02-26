---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:53.465910-07:00
description: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u3068\u306F\
  \u3001\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u3057\u3001\u6B8B\
  \u308A\u306E\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u66F4\u3059\u308B\
  \u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30C6\
  \u30AD\u30B9\u30C8\u51E6\u7406\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u6B63\
  \u898F\u5316\u3001\u7279\u5B9A\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u57FA\u6E96\
  \u3092\u6E80\u305F\u3059\u305F\u3081\u3084\u4E00\u8CAB\u6027\u3092\u78BA\u4FDD\u3059\
  \u308B\u305F\u3081\u306E\u30C7\u30FC\u30BF\u30D5\u30A9\u30FC\u30DE\u30C3\u30C6\u30A3\
  \u30F3\u30B0\u306A\u3069\u3001\u4E00\u822C\u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\
  \u3002"
lastmod: '2024-02-25T18:49:40.653212-07:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u3068\u306F\
  \u3001\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u3057\u3001\u6B8B\
  \u308A\u306E\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u66F4\u3059\u308B\
  \u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30C6\
  \u30AD\u30B9\u30C8\u51E6\u7406\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u6B63\
  \u898F\u5316\u3001\u7279\u5B9A\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u57FA\u6E96\
  \u3092\u6E80\u305F\u3059\u305F\u3081\u3084\u4E00\u8CAB\u6027\u3092\u78BA\u4FDD\u3059\
  \u308B\u305F\u3081\u306E\u30C7\u30FC\u30BF\u30D5\u30A9\u30FC\u30DE\u30C3\u30C6\u30A3\
  \u30F3\u30B0\u306A\u3069\u3001\u4E00\u822C\u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\
  \u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を大文字化するとは、最初の文字を大文字にし、残りの文字列を小文字に変更することを意味します。これは、テキスト処理、ユーザー入力の正規化、特定のフォーマット基準を満たすためや一貫性を確保するためのデータフォーマッティングなど、一般的なタスクです。

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
