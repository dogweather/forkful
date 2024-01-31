---
title:                "コマンドライン引数の読み取り"
date:                  2024-01-20T17:56:51.539295-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

category:             "Lua"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ?)
コマンドライン引数とは、プログラム起動時に外部から渡されるパラメータのこと。これを読むことで、ユーザーがプログラムの動作を動的に変えられるようになる。

## How to: (方法)
Luaでコマンドライン引数を読むのは簡単。`arg`テーブルを使えばいい。

```Lua
-- save as read_args.lua
for i = 1, #arg do
  print("Argument #" .. i .. ": " .. arg[i])
end
```

コマンドラインから実行:

```
lua read_args.lua firstArg secondArg thirdArg
```

出力例:

```
Argument #1: firstArg
Argument #2: secondArg
Argument #3: thirdArg
```

## Deep Dive (掘り下げ)
Luaでは、コマンドライン引数へアクセスするためにグローバルな`arg`テーブルが提供されている。プログラムの実行ファイル名は`arg[0]`、引数は`arg[1]`から始まる。歴史的には、Unix系OSでスクリプトに引数を渡す慣行があり、Lua言語もこの機能を取り入れている。

他言語での代替手段としては、Pythonの`sys.argv`、Rubyの`ARGV`などがある。これらも似たような使い方が可能。

Luaにおいて、引数を扱う際の実装の詳細は、通常は`arg`テーブルを使用することで十分だが、特定の環境ではOS固有のAPIコールを使う必要が出てくることもある。

## See Also (関連情報)
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)
- [Programming in Lua (first edition)](https://www.lua.org/pil/contents.html)
- Unixプログラミングの教科書やシェルスクリプトガイド

注意: リンク先の内容は時間とともに変わる可能性があるため、訪れた時の最新の情報に注意してください。
