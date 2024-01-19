---
title:                "文字列の大文字化"
html_title:           "Fish Shell: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を大文字化するとは、全ての文字を大文字に変換することを言います。プログラマーがこれを行う理由は、主にコード内の一貫性を維持し、文字列の比較を簡単にするためです。

## 実装方法:

Fish Shellで文字列を大文字化する例を見ていきましょう。

```Fish Shell
function capitalize
    echo (string upper $argv)
end
```

上記の関数はコマンドライン引数を大文字化するものです。これを "Hello world" を用いてテストしてみます。

```Fish Shell
capitalize "Hello world"
```

出力:

```Fish Shell
HELLO WORLD
```

## ディープダイブ

(1)歴史的な文脈: 文字列の大文字化は、比較を単純化し、検索を容易にし、パターンマッチングを強化するために古くから使われています。

(2)代替方法: Fish Shellには別の文字列大文字化方法があります。以下はその一例です。

```Fish Shell
echo 'Hello world' | tr '[:lower:]' '[:upper:]'
```

出力が "HELLO WORLD"になります。

(3)実装の詳細: Fish Shellの`string upper`関数はUnicode文字も大文字化できます。これにより、英語だけでなく、他の言語で書かれた文字列も適切に処理できます。

## 参考資料

- Fish Shellの文字列操作について詳しくは[公式ドキュメンテーション](https://fishshell.com/docs/current/cmds/string.html)をご覧ください。

- 文字列の大文字化について詳しく知りたい場合は、この[Stack Overflowのスレッド](https://stackoverflow.com/questions/2264428/how-to-convert-a-string-to-lower-case-in-bash)もご覧ください。