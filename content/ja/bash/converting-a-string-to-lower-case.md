---
title:    "Bash: 「文字列を小文字に変換する」"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することの利点の一つは、入力を簡単に正規化することができることです。たとえば、ユーザーが大文字で入力した場合でも、テキスト処理やデータベース検索などで期待通りに結果を得ることができます。これにより、プログラミングの作業がスムーズになります。

## 使い方

```Bash
# 変数に文字列を代入
str="Hello World"

# 文字列を小文字に変換
lower_str=${str,,}

# 結果を表示
echo $lower_str

# 出力: hello world
```

上記の例では、変数に代入された文字列を小文字に変換し、出力する方法を示しています。変換には、```${variable,,}```の形式を使用します。

## 深堀り

文字列を小文字に変換する方法には、2つの主な方法があります。1つは、上記の例で使用した```${variable,,}```を使用する方法です。もう1つは、Bashのビルトインコマンドである```tr```を使用する方法です。

```Bash
# 変数に文字列を代入
str="Hello World"

# trコマンドを使用して小文字に変換
lower_str=$(echo $str | tr '[A-Z]' '[a-z]')

# 結果を表示
echo $lower_str

# 出力: hello world
```

上記の例では、パイプを使って変数に代入された文字列を```tr```コマンドに渡し、文字を小文字に変換しています。この方法は、文字列内に特定の文字が含まれている場合に有用です。

## See Also

Markdownの基本 - https://www.markdownguide.org/basic-syntax/

BashのString Manipulation - https://www.tldp.org/LDP/abs/html/string-manipulation.html#LOWERUPPER