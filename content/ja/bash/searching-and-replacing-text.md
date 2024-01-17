---
title:                "テキストの検索と置換"
html_title:           "Bash: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何となく知ってるけど…
サーチ ＆ リプレース、複数のテキストを一括で検索し、別のテキストに置き換えること。プログラマーがそれをやる理由は簡単、大量のテキストを手作業で置き換えするのは面倒くさいから！

## やり方:
```Bash
# テキストファイル内での文字列の置き換え
sed -i 's/検索文字列/置き換え文字列/g' テキストファイル名

# 再帰的にフォルダ内のテキストファイル全てで置き換える
grep -lRZ '検索文字列' フォルダ名 | xargs -0 -l sed -i -e 's/検索文字列/置き換え文字列/g'

# 正規表現を使ってマッチする部分を置き換える
sed -i 's/[0-9].*$/置き換え文字列/g' テキストファイル名
```

## 詳しく:
その昔、テキスト編集を行う際には、手作業で単語を一つずつ置き換える必要がありました。しかし、今や私たちの助けには多様なツールやコマンドがあります。Bashコマンドでの検索と置き換えは、簡単で効率的な方法です。また、正規表現を使用することで、より柔軟にマッチングを行うことができます。代替として、sed、awk、perl、pythonなど、様々な言語でも同様の処理ができます。

## 関連情報:
- [Bashコマンドのサンプル](https://www.tldp.org/LDP/abs/html/textproc.html)
- [正規表現チートシート](https://www.rexegg.com/regex-quickstart.html)