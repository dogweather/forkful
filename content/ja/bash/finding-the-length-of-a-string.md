---
title:                "文字列の長さを見つける"
html_title:           "Bash: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ
テキストの長さを調べることの重要性は、文字列を処理するために必要な操作の一つです。例えば、文字列の一部を取り出すためには、その開始位置と長さを指定する必要があります。また、文字列の長さを調べることによって、入力が指定した長さを超えているかどうかを確認することもできます。文字列の長さを調べることは、プログラミングにおける基本的な作業の一つです。

## 使い方
文字列の長さを調べる方法は、シェルスクリプトやコマンドラインで簡単に実行することができます。

```Bash
# 変数stringに文字列を代入する
string="今日はいい天気です。"

# 文字列の長さを調べる
echo ${#string}

# 出力結果：10
```

さらに、ダブルクォーテーション内に変数名を入れることで、変数に格納された文字列の長さも調べることができます。

```Bash
# 変数nameに名前を代入する
name="山田太郎"

# 名前の長さを調べる
echo "私の名前は${#name}文字です。"

# 出力結果：私の名前は4文字です。
```

## 深堀り
文字列の長さを調べる方法には、いくつかのテクニックがあります。

まずは、文字列を変数に代入せずに直接指定する方法です。文字列をダブルクォーテーションで囲んだ後に「#」記号を付けることで、文字列の長さを調べることができます。

```Bash
# 文字列の長さを直接指定して調べる
echo ${#"こんにちは"}

# 出力結果：5
```

次に、配列を使って文字列を分割して長さを調べる方法です。配列を宣言し、文字列を分割するための区切り文字を指定して文字列を分割します。そして、配列の要素の数を調べることで文字列の長さを取得することができます。

```Bash
# 配列を使用して文字列を分割する
string="apple,banana,orange,grape"
array=(${string//,/ })

# 配列の要素数を調べることで文字列の長さを取得する
echo ${#array[@]}

# 出力結果：4
```

最後に、文字列内に含まれる特定の文字の数を調べる方法です。文字列内に含まれる特定の文字の数を調べるには、文字列に含まれるすべての文字を一つの要素として持つ配列を作成し、その配列の要素数を調べます。

```Bash
# 特定の文字の数を調べる
string="programming"
array=(${string//p/ })

# 配列の要素数を調べることで文字の数を取得する
echo ${#array[@]}

# 出力結果：3
```

## 参考リンク
- [Bash - 文字列の長さを調べる](https://www.linuxjournal.com/content/bash-powers-string-length)
- [GNU Bash - 文字列操作](https://www.gnu.org/software/bash/manual/html_node/String-Operations.html)
- [Shell Script