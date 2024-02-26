---
date: 2024-01-20 17:57:59.278779-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\u3042\
  \u308B\u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u3066\u4ED6\u306E\u6587\u5B57\u5217\
  \u306B\u5909\u3048\u308B\u64CD\u4F5C\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u306F\u30B3\u30FC\u30C9\u306E\u4FEE\u6B63\u3001\u30C7\u30FC\u30BF\u6574\u5F62\u3001\
  \u81EA\u52D5\u5316\u51E6\u7406\u3067\u3053\u308C\u3092\u3088\u304F\u4F7F\u3044\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.655965-07:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\u3042\
  \u308B\u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u3066\u4ED6\u306E\u6587\u5B57\u5217\
  \u306B\u5909\u3048\u308B\u64CD\u4F5C\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u306F\u30B3\u30FC\u30C9\u306E\u4FEE\u6B63\u3001\u30C7\u30FC\u30BF\u6574\u5F62\u3001\
  \u81EA\u52D5\u5316\u51E6\u7406\u3067\u3053\u308C\u3092\u3088\u304F\u4F7F\u3044\u307E\
  \u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

テキスト検索と置換は、ある文字列を見つけて他の文字列に変える操作です。プログラマはコードの修正、データ整形、自動化処理でこれをよく使います。

## How to: (方法)

```Fish Shell
# 文字列 'fish' を 'shark' に置換する
echo "I love fish tacos" | string replace "fish" "shark"
# 出力: I love shark tacos

# ファイル内の全 'fish' を 'shark' に置換
string replace -a -i "fish" "shark" file.txt
# file.txt 内の全ての 'fish' が 'shark' に置換される
```

## Deep Dive (深い潜水)

Fish Shellでは`string`ツールが文字列操作のために用意されています。古いシェルでは`sed`や`awk`が主流でしたが、Fishはより直観的に使えるコマンドを提供します。例えば、`string replace`は直接的な命名で何をするか明白です。実装面では、FishはUTF-8エンコーディングの文字列に対応し、設計が単純でわかりやすいです。

## See Also (関連情報)

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fish Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [GNU Sed Manual](https://www.gnu.org/software/sed/manual/sed.html) - 別の検索・置換ツール
- [AWK Programming Language](https://www.gnu.org/software/gawk/manual/gawk.html) - テキスト処理のためのプログラム言語
