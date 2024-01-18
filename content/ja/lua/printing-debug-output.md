---
title:                "デバッグ出力の印刷"
html_title:           "Lua: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

トピック：デバッグ出力の表示方法

## 何をするもの？そして何故プログラマーがそれをするのか？

デバッグ出力の表示とは、プログラムの実行中に異常やエラーが起きた際に、その情報を表示することです。プログラムの動作を理解し、問題解決のために欠かせない手段です。

## 方法：

デバッグ出力を表示するには、 `print()` 関数を使用します。例えば、以下のコードを実行すると、変数の値が表示されます。

```Lua
a = 5
print(a)
```

出力結果は以下のようになります。

```5```

## 深く掘り下げる

デバッグ出力を表示することは、プログラミングにおいて非常に重要です。デバッグ出力を利用することで、プログラムの実行中に起きる問題を特定し、修正することができます。また、プログラムの動作を理解するためにも役立ちます。他の手法としては、ブレークポイントを設定してデバッグを行う方法があります。

Luaにおいて、`print()` 関数以外にも`io.write()` 関数を使用することで、出力をファイルに書き込むこともできます。

## 関連リンク：

- [Lua公式ドキュメント（英語）](https://www.lua.org/docs.html)
- [Luaソースコード（英語）](https://github.com/lua/lua)
- [ブレークポイントとデバッグの方法（英語）](https://blog.jetbrains.com/clion/2018/04/clion-tips-tricks-breakpoint-in-lua-and-force-run-to-cursor/)