---
date: 2024-01-20 17:41:38.118885-07:00
description: "\u30D1\u30BF\u30FC\u30F3\u306B\u30DE\u30C3\u30C1\u3059\u308B\u6587\u5B57\
  \u3092\u524A\u9664\u3059\u308B\u3063\u3066\u3044\u3046\u306E\u306F\u3001\u7279\u5B9A\
  \u306E\u6761\u4EF6\u306B\u5408\u3046\u6587\u5B57\u5217\u3092\u53D6\u308A\u9664\u304F\
  \u3053\u3068\u3002\u30B3\u30FC\u30C9\u3092\u30AF\u30EA\u30FC\u30F3\u306B\u4FDD\u3063\
  \u305F\u308A\u3001\u7279\u5B9A\u306E\u30C7\u30FC\u30BF\u51E6\u7406\u3092\u884C\u3046\
  \u305F\u3081\u306B\u4F7F\u3046\u6280\u8853\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.344764-06:00'
model: gpt-4-1106-preview
summary: "\u30D1\u30BF\u30FC\u30F3\u306B\u30DE\u30C3\u30C1\u3059\u308B\u6587\u5B57\
  \u3092\u524A\u9664\u3059\u308B\u3063\u3066\u3044\u3046\u306E\u306F\u3001\u7279\u5B9A\
  \u306E\u6761\u4EF6\u306B\u5408\u3046\u6587\u5B57\u5217\u3092\u53D6\u308A\u9664\u304F\
  \u3053\u3068\u3002\u30B3\u30FC\u30C9\u3092\u30AF\u30EA\u30FC\u30F3\u306B\u4FDD\u3063\
  \u305F\u308A\u3001\u7279\u5B9A\u306E\u30C7\u30FC\u30BF\u51E6\u7406\u3092\u884C\u3046\
  \u305F\u3081\u306B\u4F7F\u3046\u6280\u8853\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
パターンにマッチする文字を削除するっていうのは、特定の条件に合う文字列を取り除くこと。コードをクリーンに保ったり、特定のデータ処理を行うために使う技術。

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
