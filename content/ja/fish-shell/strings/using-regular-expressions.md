---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:56.851676-07:00
description: "Fish Shell\u306B\u304A\u3051\u308B\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\
  \u306F\u3001\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u306B\u57FA\u3065\u3044\u3066\
  \u6587\u5B57\u5217\u3092\u691C\u7D22\u3001\u4E00\u81F4\u3055\u305B\u3001\u64CD\u4F5C\
  \u3059\u308B\u3053\u3068\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5165\u529B\u691C\u8A3C\u3001\u30D1\u30FC\u30B7\
  \u30F3\u30B0\u3001\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u306A\u3069\u306E\u30BF\u30B9\
  \u30AF\u306Bregex\u3092\u5229\u7528\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\
  \u8907\u96D1\u306A\u30C6\u30AD\u30B9\u30C8\u30D1\u30BF\u30FC\u30F3\u3092\u6307\u5B9A\
  \u3059\u308B\u305F\u3081\u306E\u30B3\u30F3\u30D1\u30AF\u30C8\u3067\u5F37\u529B\u306A\
  \u65B9\u6CD5\u3092\u63D0\u4F9B\u3059\u308B\u304B\u3089\u3067\u3059\u3002"
lastmod: 2024-02-19 22:05:01.827113
model: gpt-4-0125-preview
summary: "Fish Shell\u306B\u304A\u3051\u308B\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\
  \u306F\u3001\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u306B\u57FA\u3065\u3044\u3066\
  \u6587\u5B57\u5217\u3092\u691C\u7D22\u3001\u4E00\u81F4\u3055\u305B\u3001\u64CD\u4F5C\
  \u3059\u308B\u3053\u3068\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5165\u529B\u691C\u8A3C\u3001\u30D1\u30FC\u30B7\
  \u30F3\u30B0\u3001\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\u306A\u3069\u306E\u30BF\u30B9\
  \u30AF\u306Bregex\u3092\u5229\u7528\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\
  \u8907\u96D1\u306A\u30C6\u30AD\u30B9\u30C8\u30D1\u30BF\u30FC\u30F3\u3092\u6307\u5B9A\
  \u3059\u308B\u305F\u3081\u306E\u30B3\u30F3\u30D1\u30AF\u30C8\u3067\u5F37\u529B\u306A\
  \u65B9\u6CD5\u3092\u63D0\u4F9B\u3059\u308B\u304B\u3089\u3067\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？

Fish Shellにおける正規表現（regex）は、特定のパターンに基づいて文字列を検索、一致させ、操作することを可能にします。プログラマーは、入力検証、パーシング、テキスト処理などのタスクにregexを利用します。これは、複雑なテキストパターンを指定するためのコンパクトで強力な方法を提供するからです。

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
