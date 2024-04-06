---
date: 2024-01-20 17:41:38.118885-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.184724-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) UNIX\u3084Linux\u306E\u521D\u671F\u304B\u3089`tr`\u3084`sed`\u30B3\
  \u30DE\u30F3\u30C9\u306F\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u306B\u4F7F\u308F\u308C\
  \u3066\u3044\u308B\u3002`tr`\u306F\u30C8\u30E9\u30F3\u30B9\u30EC\u30FC\u30C8\uFF08\
  \u5909\u63DB\uFF09\u3084\u524A\u9664\u306B\u7279\u5316\u3057\u3066\u3044\u308B\u304C\
  \u3001`sed`\u306F\u30B9\u30C8\u30EA\u30FC\u30E0\u30A8\u30C7\u30A3\u30BF\u3067\u3001\
  \u3088\u308A\u8907\u96D1\u306A\u30D1\u30BF\u30FC\u30F3\u7F6E\u63DB\u3084\u524A\u9664\
  \u306B\u5F37\u3044\u3002\u78BA\u5B9F\u3067\u901F\u3044\u51E6\u7406\u304C\u5FC5\u8981\
  \u306A\u30B9\u30AF\u30EA\u30D7\u30C8\u3084\u30C7\u30FC\u30BF\u51E6\u7406\u3067\u4F7F\
  \u308F\u308C\u308B\u3002`awk`\u306A\u3069\u306E\u4ED6\u306E\u30C6\u30AD\u30B9\u30C8\
  \u51E6\u7406\u30C4\u30FC\u30EB\u3082\u3042\u308B\u304C\u3001\u8EFD\u91CF\u304B\u3064\
  \u30B7\u30F3\u30D7\u30EB\u306A\u30BF\u30B9\u30AF\u306B\u306F`tr`\u3084`sed`\u304C\
  \u3088\u304F\u7528\u3044\u3089\u308C\u308B\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

## How to: (方法)
```Bash
# 文字列から特定の文字を削除
echo "こんにちは Tokyo 2023!" | tr -d 'a-zA-Z'
# 出力: こんにちは 2023!

# 範囲を使って数字を削除
echo "こんにちは Tokyo 2023!" | tr -d '0-9'
# 出力: こんにちは Tokyo !

# 特定の文字パターンを削除
echo "これはテストです!" | sed 's/テスト//'
# 出力: これはです!
```

## Deep Dive (深堀り)
UNIXやLinuxの初期から`tr`や`sed`コマンドはテキスト処理に使われている。`tr`はトランスレート（変換）や削除に特化しているが、`sed`はストリームエディタで、より複雑なパターン置換や削除に強い。確実で速い処理が必要なスクリプトやデータ処理で使われる。`awk`などの他のテキスト処理ツールもあるが、軽量かつシンプルなタスクには`tr`や`sed`がよく用いられる。

## See Also (関連情報)
- GNU Coreutils: https://www.gnu.org/software/coreutils/manual/coreutils.html#tr-invocation
- sed manual: https://www.gnu.org/software/sed/manual/sed.html
- AWK manual: https://www.gnu.org/software/gawk/manual/gawk.html
