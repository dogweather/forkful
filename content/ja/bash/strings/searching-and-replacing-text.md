---
date: 2024-01-20 17:57:38.840608-07:00
description: "How to: (\u65B9\u6CD5) UNIX\u30B7\u30B9\u30C6\u30E0\u30671970\u5E74\u4EE3\
  \u4EE5\u6765\u4F7F\u308F\u308C\u3066\u304D\u305F\u3001`sed` (stream editor\u306E\
  \u7565) \u306F\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u3067\
  \u975E\u5E38\u306B\u6709\u529B\u3067\u3059\u3002`awk` \u3084 `grep`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.185878-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) UNIX\u30B7\u30B9\u30C6\u30E0\u30671970\u5E74\u4EE3\u4EE5\u6765\
  \u4F7F\u308F\u308C\u3066\u304D\u305F\u3001`sed` (stream editor\u306E\u7565) \u306F\
  \u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u3067\u975E\u5E38\u306B\
  \u6709\u529B\u3067\u3059\u3002`awk` \u3084 `grep` \u3068\u3044\u3063\u305F\u30D7\
  \u30ED\u30B0\u30E9\u30E0\u3082\u4F3C\u305F\u7528\u9014\u306B\u4F7F\u308F\u308C\u308B\
  \u3053\u3068\u304C\u3042\u308A\u307E\u3059\u3002\u305D\u308C\u3089\u306F\u30C6\u30AD\
  \u30B9\u30C8\u51E6\u7406\u306E\u305F\u3081\u306E\u7570\u306A\u308B\u30A2\u30D7\u30ED\
  \u30FC\u30C1\u3068\u6A5F\u80FD\u3092\u63D0\u4F9B\u3057\u307E\u3059."
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

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
