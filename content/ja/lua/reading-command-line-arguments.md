---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

コマンドライン引数の読み取りは、Luaプログラムが実行時に追加情報を受け取る方法です。これにより、システムの柔軟性と動的対応能力が高まります。

## やり方:

```Lua
-- コマンドライン引数を取得します。
local args = {...}

--引数を表示します。
for i,v in ipairs(args) do
  print("引数" .. i .. "： " .. v)
end
```
コマンドラインからこのプログラムを実行した場合：

```bash
$ lua test.lua 1 2 3
```

次の出力が得られます:

```
引数1： 1
引数2： 2
引数3： 3
```

## ディープダイブ:

コマンドライン引数の読み取りは、シェルスクリプトやC言語のような古い言語からの伝統的な概念です。
Luaでは選択肢があり、グローバルなargテーブルを使用してコマンドライン引数にアクセスすることもできます。この方法の詳細については、公式ドキュメンテーションを参照してください。
```Lua
-- コマンドライン引数を取得します。
for i,v in ipairs(arg) do
  print("引数" .. i .. "： " .. v)
end
```

## 参考資料：

- コマンドライン引数についてのより詳細な議論: [https://learnxinyminutes.com/docs/lua/](https://learnxinyminutes.com/docs/lua/)
- Lua言語の公式ドキュメンテーション: [http://www.lua.org/manual/5.4/manual.html](http://www.lua.org/manual/5.4/manual.html)