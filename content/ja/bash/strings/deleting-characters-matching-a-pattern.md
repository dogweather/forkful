---
title:                "パターンに一致する文字を削除する"
aliases:
- /ja/bash/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:41:38.118885-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/deleting-characters-matching-a-pattern.md"
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
