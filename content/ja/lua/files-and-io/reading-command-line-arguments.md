---
date: 2024-01-20 17:56:51.539295-07:00
description: "How to: (\u65B9\u6CD5) Lua\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\
  \u30F3\u5F15\u6570\u3092\u8AAD\u3080\u306E\u306F\u7C21\u5358\u3002`arg`\u30C6\u30FC\
  \u30D6\u30EB\u3092\u4F7F\u3048\u3070\u3044\u3044\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.170341-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Lua\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\
  \u6570\u3092\u8AAD\u3080\u306E\u306F\u7C21\u5358\u3002`arg`\u30C6\u30FC\u30D6\u30EB\
  \u3092\u4F7F\u3048\u3070\u3044\u3044\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
