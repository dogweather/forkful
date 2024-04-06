---
date: 2024-01-26 03:45:58.494627-07:00
description: "\u65B9\u6CD5\uFF1A Lua\u306F\u4ED6\u306E\u8A00\u8A9E\u3068\u9055\u3044\
  \u3001\u6700\u521D\u304B\u3089\u4E38\u3081\u95A2\u6570\u3092\u542B\u3093\u3067\u3044\
  \u307E\u305B\u3093\u3002\u6B74\u53F2\u7684\u306B\u306F\u3001\u81EA\u5206\u3067\u66F8\
  \u304F\u304B\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u3046\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002\u4E00\
  \u822C\u7684\u306A\u56DE\u907F\u7B56\u306F\u3001`math.floor()` \u3092\u4F7F\u7528\
  \u3057\u3066\u5207\u308A\u6368\u3066\u3001 `math.ceil()`\u2026"
lastmod: '2024-04-05T22:38:41.824315-06:00'
model: gpt-4-0125-preview
summary: "\u72EC\u81EA\u306E\u95A2\u6570\u3092\u4F5C\u6210\u3059\u308B\u4EE3\u308F\
  \u308A\u306B\u300Clua-users wiki\u300D\u3084\u300CPenlight\u300D\u306A\u3069\u306E\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3082\u3067\
  \u304D\u307E\u3059\u3002\u305D\u308C\u305E\u308C\u306B\u306F\u8FFD\u52A0\u6A5F\u80FD\
  \u3084\u30AA\u30FC\u30D0\u30FC\u30D8\u30C3\u30C9\u306E\u5897\u52A0\u306A\u3069\u3001\
  \u5229\u70B9\u3068\u30C8\u30EC\u30FC\u30C9\u30AA\u30D5\u304C\u3042\u308A\u307E\u3059\
  \u3002"
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

## 方法：
```lua
-- Luaには基本的な丸め処理機能が組み込まれていませんが、関数を定義することができます：

function round(num)
    return num >= 0 and math.floor(num + 0.5) or math.ceil(num - 0.5)
end

print(round(3.5))  -- 4
print(round(2.3))  -- 2
print(round(-1.6)) -- -2

-- 特定の小数点以下で丸めるには：
function round(num, decimalPlaces)
    local mult = 10^(decimalPlaces or 0)
    return math.floor(num * mult + 0.5) / mult
end

print(round(3.14159, 2)) -- 3.14
print(round(1.98765, 3))  -- 1.988
```

## 詳細解説
Luaは他の言語と違い、最初から丸め関数を含んでいません。歴史的には、自分で書くか、サードパーティのライブラリを使う必要があります。一般的な回避策は、`math.floor()` を使用して切り捨て、 `math.ceil()` を使用して切り上げることで、これを行う前に数値の符号に応じて0.5を足したり引いたりします。

独自の関数を作成する代わりに「lua-users wiki」や「Penlight」などのライブラリを使用することもできます。それぞれには追加機能やオーバーヘッドの増加など、利点とトレードオフがあります。

内部では、これらの関数は通常、コンピュータが浮動小数点数を格納する方法を利用して動作します。丸めたい正の浮動小数点数に0.5を加えると次の整数値の閾値を超えるので、`math.floor()`を適用するとその最も近い整数に丸められます。

## 参照
- [Lua 5.4 リファレンスマニュアル: 数学関数](https://www.lua.org/manual/5.4/manual.html#6.7)
- [Penlight Luaライブラリ: Math](https://github.com/lunarmodules/Penlight)
