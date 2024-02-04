---
title:                "正規表現の使用"
date:                  2024-02-03T19:16:56.851676-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
