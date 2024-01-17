---
title:                "文字列の先頭を大文字にする"
html_title:           "Fish Shell: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ファーストシェルで文字列を大文字にする方法

### 何が & どうして?

文字列を大文字にすることは、文字列中の全ての文字を大文字に変換することを意味します。プログラマーがこのようなことをする理由は、データの整理や比較を容易にするためです。例えば、ファイルやデータベース内の同じ文字列が大文字と小文字で異なるものとして処理されてしまう場合、文字列をすべて大文字にすることで正しい比較ができるようになります。

## やり方: 

文字列を大文字にするには、以下のようにコードを書きます。

```
fish_shell -c "set -U string "hello world""
fish_shell -c "echo $string | tr '[:lower:]' '[:upper:]'"
```

`set -U string` を使って `hello world` を設定し、その後 `echo` と `tr` を使って大文字に変換します。

## ディープダイブ:

### 過去のコンテクスト:

文字列を大文字に変換する方法は、昔からあります。古い言語では、大文字と小文字の区別がない場合もあり、大文字に変換する必要がありました。しかし、現代の言語では大文字と小文字の区別をすることができるため、この方法は必ずしも必要ではありません。

### 代替方法:

文字列を大文字に変換する代替方法としては、 `sed` コマンドや `awk` コマンドを使うことができます。これらのコマンドは、テキスト操作に特化しており、一部のプログラムでは依存性として使用されていることがあります。

### 実装の詳細:

Fish Shell は、文字列を大文字に変換するために内部的に `tr` コマンドを使用しています。このコマンドは、標準入力から受け取った文字列を大文字に変換する機能を持っています。

## 関連リンク:

- [Fish Shell documentations - string interpolation and manipulation](https://fishshell.com/docs/current/cmds/set.html)
- [sed command](https://www.gnu.org/software/sed/)
- [awk command](https://www.gnu.org/software/gawk/)