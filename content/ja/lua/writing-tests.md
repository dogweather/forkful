---
title:                "テストの作成"
date:                  2024-01-19
html_title:           "Bash: テストの作成"
simple_title:         "テストの作成"

category:             "Lua"
tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テストとは、コードが期待通りに動くことを保証する手法です。品質を保ち、将来のバグやエラーを防ぐためにプログラマーが行います。

## How to: (やり方)
Luaには標準のテストフレームワークがないため、`busted`や`luaunit`などの外部ライブラリを使う。以下は`luaunit`を使った簡単なテスト例です。

```Lua
local luaunit = require('luaunit')
local mymodule = require('mymodule')

function testAdd()
    luaunit.assertEquals(mymodule.add(1, 1), 2)
end

os.exit(luaunit.LuaUnit.run())
```

実行結果は以下の通りです。

```
..
Ran 1 test in 0.001 seconds
OK
```

## Deep Dive (詳細情報)
テストの記述は、1990年代にソフトウェア開発の一部となりました。Lua言語は比較的新しい言語なので、テストエコシステムも進化し続けています。`busted`はBDDスタイルのテストを、`luaunit`はxUnitスタイルのテストをサポートします。Luaにおけるテストは、主に独立したライブラリを使用して行うのが一般的ですが、`TAP`や`TestMore`のようなプロトコルを支持するものもあります。

## See Also (参照)
- [`luaunit`](https://github.com/bluebird75/luaunit) – luaunitのGitHubリポジトリ。
