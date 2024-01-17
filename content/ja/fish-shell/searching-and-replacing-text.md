---
title:                "テキストの検索と置換"
html_title:           "Fish Shell: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何か&なんで?

テキストの検索と置換とは何かを説明し、プログラマーがそれをする理由を説明します。

##やり方:

```
Fish Shellを使用して、テキストの検索と置換を行う方法を以下のコーディング例と出力のサンプルを用いて説明します。

```

### 文字列の検索と置換

```
#文字列の検索と置換
set my_string "こんにちは!"
echo $my_string
# 出力: こんにちは!

# 文字列の置換
set new_string (echo $my_string | sed 's/こんにちは/Hello/')
echo $new_string
#出力: Hello!
```

### ファイル内のテキストの検索と置換

```
#ファイル内のテキストの検索と置換
set file_name my_file.txt

# 検索したい文字列
set search_string "Hello"
# 置換したい文字列
set replace_string "こんにちは"

# ファイル内のテキストを検索し、置換する
sed -i 's/'$search_string'/'$replace_string'/g' $file_name

# 置換後のファイルの中身を確認
cat $file_name
```

## 詳細を見る:

### 歴史的な文脈

テキストの検索と置換は、1970年代に最初に登場したUnixのツールであるsed (Stream Editor)によって導入されました。その後、他のツールやプログラミング言語でも同様の機能が実装されました。

### 代替手段

Fish Shellを使用せずにテキストの検索と置換を行うには、他のシェルやテキストエディタ、プログラミング言語を使用することができます。例えば、bashシェルでは、`sed`コマンドを使用してテキストの検索と置換を実行できます。

### 実装の詳細

Fish Shellは、検索と置換のために標準のUnixツールであるsedとawkを使用しています。これらのツールは、正規表現やパターンマッチングを使用してテキストを検索・置換することができます。

## 関連情報を見る:

- [fish shell公式ウェブサイト](https://fishshell.com/)
- [Fish Shell on GitHub](https://github.com/fish-shell/fish-shell)
- [sed - Linux Command](https://www.computerhope.com/unix/used.htm)