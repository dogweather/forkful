---
title:                "コードを関数に整理する"
aliases:
- ja/fish-shell/organizing-code-into-functions.md
date:                  2024-01-28T23:01:54.914439-07:00
model:                 gpt-4-0125-preview
simple_title:         "コードを関数に整理する"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, dogweather, reviewed and added links
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
コードを関数にまとめることは、特定のタスクを実行するためのスクリプトのビットを束ねることについてです。これを行う理由は、コードを読みやすく、テストしやすく、再利用しやすくするためです。誰もがコードスパゲッティの沼を歩きたいと思っているわけではありません。

## 方法：
Fishで関数を書くには、`function`キーワードを使い、名前を付け、`end`で終了します。こちらが簡単なものです：

```fish
function hello
    echo "Hello, World!"
end

hello
```

出力：
```
Hello, World!
```

では、ユーザーにあいさつするようにしましょう：

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

出力：
```
Hey there, your_username!
```

セッションを跨いで保存するには、`funcsave greet`を使用します。

## 深掘り
Fish Shellの関数はミニスクリプトのようなものです。あらゆるものを詰め込むことができます。歴史的に、シェルスクリプティングの関数の概念は、繰り返しのタイピングとデバッグの何千時間も節約してきました。Pythonのようなプログラミング言語とは異なり、Shellの関数は構造よりも便利さについてです。

一部のシェル、例えばBashは、`function`または単に中括弧を使用します。Fishは`function ... end`に固執しています。明確で読みやすいです。Fish関数の内部では、パラメーター、`set -l`でのローカル変数、さらには別の関数内で関数を定義することさえできます。

`return`値は必要ありません。Fishはそれを重視していないからです。関数の出力がその返り値です。そして、将来のセッションで利用可能な永続的な関数を望むならば、`funcsave`を覚えておいてください。

## 参照

- 関数に関するfishチュートリアル: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### 関数コマンド

- [function](https://fishshell.com/docs/current/cmds/function.html) — 関数を作成
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — 関数を表示または消去
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — 関数の定義をユーザーの自動読み込みディレクトリに保存
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — 関数を対話的に編集
