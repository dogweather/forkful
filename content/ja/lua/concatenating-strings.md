---
title:                "「文字列の連結」"
html_title:           "Lua: 「文字列の連結」"
simple_title:         "「文字列の連結」"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何それ？
文字列の連結とは、Luaプログラムで文字列を結合することです。プログラマーがこの作業を行う理由は、複数の文字列を組み合わせてより複雑なテキストを作成するためです。

## 方法：
「Lua …」のコードブロック内にコーディング例とサンプル出力を示します。

### 単純な文字列の連結
```Lua
local str1 = "こんにちは"
local str2 = "世界"
local result = str1 .. str2
print(result)

-- こんにちは世界
```

### 数値との連結
```Lua
local num = 123
local str = "番目の数字は"
local result = num .. "番目の数字は"
print(result)

-- 123番目の数字は
```

### 複数の文字列と数値の連結
```Lua
local word = "私は"
local num = 24
local age = "歳です"
local result = word .. num .. age
print(result)

-- 私は24歳です
```

## 詳細を深く掘り下げる
### 歴史的な背景
文字列の連結は、プログラム言語で広く使用されている機能です。昔ながらのプログラミング言語であるFortranは、文字列の連結を処理するために、桁数を考慮した手法を使用していました。現代のプログラミング言語では、この機能はより簡単に使用できるように改善されています。

### 代替手段
文字列の連結機能は、その他の方法でも実現することができます。たとえば、文字列内に変数を埋め込んで連結することができます。しかし、この方法は複雑でミスの元になる可能性があります。

### 実装の詳細
Luaでは、文字列の連結に「..」という記号を使用します。この記号は、左の文字列と右の文字列を結合するという意味になります。文字列を別のデータ型と連結する場合、自動的に文字列に変換されます。

## 関連情報を見る
- [Lua公式サイト](https://www.lua.org/)
- [文字列の連結についてのドキュメント](https://www.lua.org/manual/5.4/manual.html#3.4.6)
- [Luaで文字列を処理する方法](https://qiita.com/ry.10201121/items/06d22cb292f3103252ed)