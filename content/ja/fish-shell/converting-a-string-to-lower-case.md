---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列を小文字に変換するとは、コンピュータプログラム内のすべての大文字を小文字に変換する処理を指します。これはデータの一貫性を保つため、また、ユーザ入力の形式に関係なく検索や比較を行うためによく行われます。

## どうやって：
Fish Shellでは、`string lower`関数を使用して文字列を小文字に変換します。以下に例を示します：

```Fish Shell
set text 'HELLO WORLD'
echo $text | string lower
```

出力は以下の通りです：
```
hello world
```

## ディープダイブ
文字列を小文字に変換する概念は古くから存在しており、古いプログラミング言語でも見つけることができます。Fish Shellでは、`string lower`関数を実装するためにC++の`std::tolower`関数を使用しています。他の方法としては、`tr`コマンドを使用したUNIXスタイルの方法や、PythonやJavaScriptのような他のプログラミング言語で提供されている組み込み関数を使用する方法があります。

## 参照情報
Fish Shellの詳細については、[公式ドキュメンテーション](https://fishshell.com/docs/current/index.html)を参照してください。他のプログラミング言語で文字列を小文字に変換する方法については、[StackOverflow](https://stackoverflow.com/questions/tagged/string+lowercase)の関連スレッドが参考になります。