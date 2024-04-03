---
date: 2024-01-20 17:41:38.118885-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.344764-06:00'
model: gpt-4-1106-preview
summary: .
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
