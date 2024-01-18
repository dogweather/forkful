---
title:                "コンピュータープログラミングにおける「コマンドライン引数の読み取り」"
html_title:           "Lua: コンピュータープログラミングにおける「コマンドライン引数の読み取り」"
simple_title:         "コンピュータープログラミングにおける「コマンドライン引数の読み取り」"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なに？なぜ？

コマンドライン引数を読み込むこととは、プログラマーがプログラム内の特定のコードにアクセスすることができるように情報を提供することです。プログラマーがユーザーからの入力データを受け取るために使われ、特にユーザーによって変更される可能性のあるプログラムの場合に重要です。

## 方法：

```Lua
-- 単純なコマンドライン引数の読み込み
local argument = arg[1]
print(argument)

-- 複数のコマンドライン引数の読み込み
local arg_length = #arg
for i = 1, arg_length do
  local argument = arg[i]
  print(argument)
end
```

出力:
```bash
$ lua example.lua hello
hello

$ lua example.lua hello world
hello
world
```

## 詳細：

コマンドライン引数の読み込みは、プログラミング言語によって異なります。Luaでは、そのまま ```arg``` というグローバル変数を使用することで、コマンドライン引数にアクセスすることができます。また、コマンドライン引数は全て文字列として読み込まれます。

別の方法としては、Luaの外部ライブラリである ```lua-apr``` を使用することで、より高度なコマンドライン引数の読み込みが可能です。 ```lua-apr``` は、Perlの ```Getopt::Long``` モジュールと似たインターフェースを提供します。

## 関連情報：

- [lua-users wiki - コマンドライン引数の読み込み](http://lua-users.org/wiki/CommandLineArguments)
- [lua-apr](https://github.com/mwild1/lua-apr)