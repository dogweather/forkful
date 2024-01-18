---
title:                "正規表現の使用"
html_title:           "Lua: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何？ & なぜ？

正規表現とは、テキスト内の特定のパターンを検索するために使用されるプログラミングツールです。プログラマーたちは正規表現を使用することで、大量のデータの中から特定のパターンを効率的に抽出することができます。

## 方法：

```Lua
-- テキスト内の数字を検索する
local text = "今日は2021年11月11日です。"
local pattern = "%d+"
local result = string.match(text, pattern)

-- 結果：2021
```

```Lua
-- テキスト内の特定の文字列を置換する
local text = "こんにちは、私の名前はLuaです。"
local pattern = "Lua"
local replacement = "Python"
local result = string.gsub(text, pattern, replacement)

-- 結果：こんにちは、私の名前はPythonです。
```

```Lua
-- テキスト内の特定のパターンを抽出する
local text = "ご注文番号：AB12345678"
local pattern = "[%u%d]+"
local result = string.match(text, pattern)

-- 結果：AB12345678
```

## ディープダイブ：

正規表現は、1960年代に誕生しました。今日では、ほぼすべてのプログラミング言語でサポートされており、非常に便利なツールとなっています。正規表現を使用しない場合、同じ作業をするために大量のコードを書く必要があるかもしれません。代替手段として、文字列操作関数などがありますが、複雑なパターンを簡単に検索することはできません。正規表現は、パターンマッチングのために特別に設計されており、これらの問題を解決するために最適なツールです。

## 参考サイト：

- [Lua正規表現チュートリアル](https://lua-users.org/wiki/PatternsTutorial)
- [正規表現クイックリファレンス](https://www.regular-expressions.info/quickstart.html)
- [正規表現でパターンマッチングを行うメリット](https://www.homeandlearn.co.uk/java/string_regex_and_matching.html)