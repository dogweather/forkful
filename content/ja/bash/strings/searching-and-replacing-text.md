---
date: 2024-01-20 17:57:38.840608-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\
  \u6307\u5B9A\u3055\u308C\u305F\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\
  \u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u3001\u305D\u308C\u3092\u65B0\u3057\u3044\
  \u6587\u5B57\u5217\u3067\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u306E\u4FEE\u6B63\
  \u3001\u30C7\u30FC\u30BF\u306E\u6574\u5F62\u3001\u3042\u308B\u3044\u306F\u5358\u7D14\
  \u306B\u60C5\u5831\u306E\u66F4\u65B0\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.904781-06:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\
  \u6307\u5B9A\u3055\u308C\u305F\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\
  \u6587\u5B57\u5217\u3092\u898B\u3064\u3051\u3001\u305D\u308C\u3092\u65B0\u3057\u3044\
  \u6587\u5B57\u5217\u3067\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u306E\u4FEE\u6B63\
  \u3001\u30C7\u30FC\u30BF\u306E\u6574\u5F62\u3001\u3042\u308B\u3044\u306F\u5358\u7D14\
  \u306B\u60C5\u5831\u306E\u66F4\u65B0\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
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
