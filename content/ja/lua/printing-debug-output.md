---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/printing-debug-output.md"
---

{{< edit_this_page >}}

# Luaにおけるデバッグ出力のプリント方法

## 何何何何？ そしてなぜ？
デバッグ出力は、エラーの調査や状態の確認するためにプログラムが表示する情報です。プログラマーが挙動を理解しやすくするために使用します。

## どうやって：
Luaでは`print`関数を使ってデバッグ出力を表示できます。例えば：

```Lua
print("Hello, world!")
local num = 42
print("The number is: " .. num)
```
このコードは
```
Hello, world!
The number is: 42
```
という結果が出力されます。

## ディープダイブ：
イェール人にとって、`print`関数を使用したデバッグ出力は非常に簡単で直感的な方法です。しかし、Luaが提供する唯一の方法ではありません。

Luaは元々1993年にリリースされ、その設計哲学の一部はシンプルさと柔軟性です。よって、標準ライブラリは極めて制限されています。その結果として、より高度なデバッグ出力には`io.write`を中心とした手法やサードパーティモジュールへとシフトしています。

`print`関数は内部的に`tostring`を使用して渡された値を文字列に変換し、それを標準出力に書き出します。この特性により、任意の値を簡単にデバッグ出力として表示することが可能です。

## もっと見る：
- Luaの公式ドキュメンテーション: [print関数](https://www.lua.org/manual/5.4/manual.html#6.1)
- Lua-users wiki: [入出力チュートリアル](http://lua-users.org/wiki/IoLibraryTutorial) 
- Stack Overflow: [Luaでのprintfの使用](https://stackoverflow.com/questions/10434599/how-to-get-lua-print-function-to-work-in-the-correct-order)