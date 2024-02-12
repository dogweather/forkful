---
title:                "テキストの検索と置換"
aliases:
- /ja/bash/searching-and-replacing-text.md
date:                  2024-01-20T17:57:38.840608-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストの検索と置換は、指定されたパターンに一致する文字列を見つけ、それを新しい文字列で置き換えることです。プログラマーは、コードの修正、データの整形、あるいは単純に情報の更新のためにこれを行います。

## How to: (方法)
```Bash
# 文字列 'hello' を 'world' に置換する
echo "hello, hello!" | sed 's/hello/world/'
```
出力:
```
world, hello!
```

```Bash
# すべての出現箇所を置換する ('g'オプション)
echo "hello, hello!" | sed 's/hello/world/g'
```
出力:
```
world, world!
```

```Bash
# ファイル内のテキストを置換する
sed -i 's/old_text/new_text/g' example.txt
```

## Deep Dive (掘り下げ)
UNIXシステムで1970年代以来使われてきた、`sed` (stream editorの略) はテキストの検索と置換で非常に有力です。`awk` や `grep` といったプログラムも似た用途に使われることがあります。それらはテキスト処理のための異なるアプローチと機能を提供します。

`sed` はパイプライン処理と組み合わせることができ、スクリプトでの自動化に適しています。`-i` オプションはファイル内の操作を直接行い、新しいファイルを作成することなく変更が適用されます。

検索と置換は正規表現と組み合わせて強化され、さらに複雑なパターンマッチングとテキスト操作が可能になります。例えば、次のようにしてメールアドレスのドメインを変更することができます。

```Bash
echo "name@example.com" | sed 's/@example\.com/@newdomain\.com/'
```

`sed`はその強力さゆえ、複雑なパターンや大きなファイルを扱う際には注意が必要です。不正確な正規表現は予期せぬ結果を生むことがあります。また、特に大規模なファイルを扱うときは、実行速度が低下する可能性があります。

## See Also (参照)
- GNU `sed` マニュアル: https://www.gnu.org/software/sed/manual/sed.html
- `awk` プログラミング言語: https://www.gnu.org/software/gawk/manual/gawk.html
- `grep` マニュアル: http://www.gnu.org/software/grep/manual/grep.html
- 正規表現について学ぶ: https://www.regular-expressions.info/
