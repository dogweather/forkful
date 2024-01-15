---
title:                "文字列を小文字に変換する"
html_title:           "Bash: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換する作業は、プログラミングで様々な用途に応用することができます。例えば、文字列の比較や検索の際に大文字と小文字を区別しないようにするためなどが挙げられます。

## 使い方
文字列を小文字に変換するには、Bashの`tr`コマンドを使用します。以下のような形式で記述します。

```Bash 
echo "SOME STRING" | tr "[:upper:]" "[:lower:]"
```

この例では、`tr`コマンドを使用して大文字を小文字に変換しています。実行すると、出力は`some string`となります。

## ディープダイブ
文字列を小文字に変換する際には、様々なオプションが存在します。例えば、`-d`オプションを使用すると、変換する文字列を指定することができます。また、`-s`オプションを使用することで、連続した文字を1つの文字にまとめることができます。さらに、正規表現を使用することでも変換の幅を広げることができます。

See Also
- Bashの`tr`コマンドマニュアル (https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- 正規表現を使用した文字列変換方法 (https://www.gnu.org/software/sed/manual/html_node/The-_002dE-Option.html#The-_002dE-Option)
- Bashのテキスト処理コマンド一覧 (https://www.gnu.org/software/bash/manual/html_node/Text-Processing.html)