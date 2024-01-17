---
title:                "文字列の抽出"
html_title:           "Bash: 文字列の抽出"
simple_title:         "文字列の抽出"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何をするのか？
サブストリングを抽出するとは、文字列の一部を取り出し、それを新しい文字列として使用することです。プログラマーは、文字列をより細かく操作する必要がある場合に、サブストリングを抽出します。

## 方法:
```Bash
# 変数に文字列を代入
str="こんにちは、こんにちは"

# 変数の先頭から4文字のサブストリングを抽出
echo "${str:0:4}" 
# 出力結果: こんに

# 変数の3番目の文字から最後までのサブストリングを抽出
echo "${str:2}" 
# 出力結果: んにちは、こんにちは
``` 

## 詳細を深める:
サブストリングを抽出するとき、最初の引数は抽出したい文字列の位置を指定し、2番目の引数は抽出する文字数を指定します。Bashでは、カットオフポイントを指定するために、文字列を0から始めるインデックス番号を使用します。つまり、1番目の文字は0番目の文字として指定されます。
サブストリングを抽出する代わりの方法として、パターンマッチングや正規表現を使用することもできます。これにより、より柔軟な文字列の抽出が可能になります。
Bashでは、サブストリングを抽出する際に使用されるコマンドは「${var:offset:length}」です。varは変数名、offsetは抽出したい文字列の位置、lengthは抽出する文字数を表します。

## 関連情報:
- [Bashのサブストリング抽出](https://www.shellscript.sh/tips/substring/)
- [Bashのパターンマッチング](https://linuxize.com/post/bash-pattern-matching/)
- [Bashの正規表現](https://linuxhint.com/using_bash_regular_expressions/)