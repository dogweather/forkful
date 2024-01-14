---
title:                "Fish Shell: 文字列の連結"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
文字列の連結に取り組むのかについては、言語の書式設定や読みやすさを向上させるために必要だからです。文字列の連結をすることで、複数の情報を1つにまとめることができ、コードをより効率的に管理することができます。

## 方法
````Fish Shell````を使用して、簡単なコードを書き、実行する方法を説明します。まず、複数の文字列を定義します。
````fish
set name "山田"
set greeting "こんにちは"
set age 25
````
そして、````echo````コマンドを使用して、````concat````関数を使用することで、これらの文字列を連結します。
````fish
function concat
    echo $greeting", "$name"さん。あなたは"$age"歳です。"
end
````
実行結果は以下のようになります。
````fish
¥ concat
こんにちは、山田さん。あなたは25歳です。
````

## 深堀り
文字列の連結には、````fish````の便利な機能があります。例えば、````string join````を使用することで、配列内の文字列を連結することができます。また、````math````関数を使用することで、文字列内に式を埋め込むことができます。

## See Also
- [Official Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Concatenating Strings in Bash Shell](https://linuxhint.com/concatenate_strings_bash/)
- [Introduction to Markdown (Japanese)](https://www.markdown.jp/what-is-markdown/)