---
title:                "コードを関数に整理する"
date:                  2024-01-26T01:10:06.080317-07:00
model:                 gpt-4-1106-preview
simple_title:         "コードを関数に整理する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 何となぜ？
コードを関数にまとめるというのは、特定のタスクを実行するスクリプトのビットをまとめることを意味します。それを行う理由は、コードを読みやすく、テストしやすく、再利用しやすくするためです。誰もがコードのスパゲティの沼を渡り歩きたいとは思いません。

## 方法:
Fishでは、`function`キーワードで関数を書き、名前を付けて、`end`で終わります。こちらがシンプルな例です:

```fish
function hello
    echo "Hello, World!"
end

hello
```

出力:
```
Hello, World!
```

次に、ユーザーにあいさつするようにしてみましょう:

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

出力:
```
Hey there, your_username!
```

セッション間で保存するには、`funcsave greet`を使います。

## 詳細解説
Fish Shellの関数はミニスクリプトのようなものです。ほぼどんなものでもそこに入れることができます。歴史的には、シェルスクリプティングにおける関数の概念は、繰り返しのタイピングやデバッグにかける時間を数えきれないほど節約してくれています。Pythonのようなプログラミング言語とは異なり、シェル関数は構造よりも便宜についてです。

Bashのようなシェルでは、`function`や単純な中括弧を使います。Fishは`function ... end`を採用しています — クリアで読みやすいです。Fish関数の内部では、引数や、`set -l`でのローカル変数、さらには別の関数の中で関数を定義することも可能です。

`return`値は必要ないです、なぜならFishではそれに大きく依存していないからです; 関数の出力がそのリターン値です。そして、将来のセッションでも利用可能な永続的な関数を作りたい場合は、`funcsave`を覚えておいてください。

## 参考
- 関数に関するFishチュートリアル: https://fishshell.com/docs/current/tutorial.html#tut_functions
- `function`に関するFishドキュメンテーション: https://fishshell.com/docs/current/cmds/function.html
- Fishで関数を書くための広範なガイド: https://fishshell.com/docs/current/index.html#syntax-function
