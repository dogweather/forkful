---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:01:54.914439-07:00
description: "\u65B9\u6CD5\uFF1A Fish\u3067\u95A2\u6570\u3092\u66F8\u304F\u306B\u306F\
  \u3001`function`\u30AD\u30FC\u30EF\u30FC\u30C9\u3092\u4F7F\u3044\u3001\u540D\u524D\
  \u3092\u4ED8\u3051\u3001`end`\u3067\u7D42\u4E86\u3057\u307E\u3059\u3002\u3053\u3061\
  \u3089\u304C\u7C21\u5358\u306A\u3082\u306E\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.744402-06:00'
model: gpt-4-0125-preview
summary: "Fish\u3067\u95A2\u6570\u3092\u66F8\u304F\u306B\u306F\u3001`function`\u30AD\
  \u30FC\u30EF\u30FC\u30C9\u3092\u4F7F\u3044\u3001\u540D\u524D\u3092\u4ED8\u3051\u3001\
  `end`\u3067\u7D42\u4E86\u3057\u307E\u3059\u3002\u3053\u3061\u3089\u304C\u7C21\u5358\
  \u306A\u3082\u306E\u3067\u3059\uFF1A."
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
weight: 18
---

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
