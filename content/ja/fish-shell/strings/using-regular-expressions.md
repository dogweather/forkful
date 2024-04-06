---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:56.851676-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Fish Shell\u81EA\u4F53\
  \u306B\u306Fregex\u306E\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u30B3\u30DE\u30F3\
  \u30C9\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001`grep`\u3001`sed`\u3001`awk`\u306E\
  \u3088\u3046\u306A\u5916\u90E8\u30B3\u30DE\u30F3\u30C9\u3092\u52B9\u679C\u7684\u306B\
  \u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u308C\u3089\u306E\u30B3\u30DE\u30F3\u30C9\
  \u306Fregex\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u304A\u308A\u3001\u30B9\u30AF\
  \u30EA\u30D7\u30C8\u306Bregex\u64CD\u4F5C\u3092\u7D44\u307F\u8FBC\u3080\u3053\u3068\
  \u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:43.504155-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## どのようにして：
Fish Shell自体にはregexのための組み込みコマンドはありませんが、`grep`、`sed`、`awk`のような外部コマンドを効果的に使用します。これらのコマンドはregexをサポートしており、スクリプトにregex操作を組み込むことを可能にします。

### `grep`による基本的なパターンマッチング
ファイル内のパターンに一致する行を検索する：

```fish
grep '^[0-9]+' myfile.txt
```

このコマンドは`myfile.txt`で、1つ以上の数字で始まる行を見つけます。

### `sed`による抽出と置換
ファイルから電話番号を抽出する：

```fish
sed -n '/\([0-9]\{3\}\)-\([0-9]\{3\}\)-\([0-9]\{4\}\)/p' contacts.txt
```

`data.txt`内の"foo"のすべての発生を"bar"に置換する：

```fish
sed 's/foo/bar/g' data.txt
```

### 基本的なRegexのための`string`の使用
Fish Shellの`string`コマンドは、マッチや置換のようなシンプルなregex操作をサポートしています：

文字列内のパターンをマッチ：

```fish
echo "fish 3.1.2" | string match -r '3\.[0-9]+\.[0-9]+'
```
出力：
```
3.1.2
```

'fish'に続く数字を'X.X.X'に置換する：

```fish
echo "Welcome to fish 3.1.2" | string replace -ra '([fish]+\s)[0-9\.]+' '$1X.X.X'
```
出力：
```
Welcome to fish X.X.X
```

### `awk`による高度なマッチング
最初の列が特定のパターンに一致する場合に第二列のデータを出力する：

```fish
awk '$1 ~ /^a[0-9]+$/ {print $2}' datafile
```

このコマンドは`datafile`で最初の列が"a"に続く1つ以上の数字で始まる行を探し、第二列を出力します。

これらの外部コマンドを統合することで、Fish Shellプログラマーは複雑なテキスト操作タスクのために正規表現の全力を活用でき、シェルのネイティブ機能を強化できます。
