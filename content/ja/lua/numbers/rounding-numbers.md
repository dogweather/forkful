---
date: 2024-01-26 03:45:58.494627-07:00
description: "\u6570\u5024\u3092\u4E38\u3081\u308B\u3068\u306F\u3001\u6700\u3082\u8FD1\
  \u3044\u6574\u6570\u307E\u305F\u306F\u6307\u5B9A\u3055\u308C\u305F\u5C0F\u6570\u70B9\
  \u4EE5\u4E0B\u3067\u8ABF\u6574\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u3053\u308C\u306F\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\
  \u3044\u3066\u8907\u96D1\u3055\u3092\u6E1B\u3089\u3057\u3001\u30D1\u30D5\u30A9\u30FC\
  \u30DE\u30F3\u30B9\u3092\u5411\u4E0A\u3055\u305B\u3001\u7279\u5B9A\u306E\u30DD\u30A4\
  \u30F3\u30C8\u3092\u8D85\u3048\u308B\u7CBE\u5EA6\u304C\u4FA1\u5024\u3092\u52A0\u3048\
  \u306A\u3044\u5834\u5408\u306B\u884C\u3046\u6A19\u6E96\u7684\u306A\u51E6\u7406\u3067\
  \u3059\u3002"
lastmod: 2024-02-19 22:05:01.432998
model: gpt-4-0125-preview
summary: "\u6570\u5024\u3092\u4E38\u3081\u308B\u3068\u306F\u3001\u6700\u3082\u8FD1\
  \u3044\u6574\u6570\u307E\u305F\u306F\u6307\u5B9A\u3055\u308C\u305F\u5C0F\u6570\u70B9\
  \u4EE5\u4E0B\u3067\u8ABF\u6574\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u3053\u308C\u306F\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\
  \u3044\u3066\u8907\u96D1\u3055\u3092\u6E1B\u3089\u3057\u3001\u30D1\u30D5\u30A9\u30FC\
  \u30DE\u30F3\u30B9\u3092\u5411\u4E0A\u3055\u305B\u3001\u7279\u5B9A\u306E\u30DD\u30A4\
  \u30F3\u30C8\u3092\u8D85\u3048\u308B\u7CBE\u5EA6\u304C\u4FA1\u5024\u3092\u52A0\u3048\
  \u306A\u3044\u5834\u5408\u306B\u884C\u3046\u6A19\u6E96\u7684\u306A\u51E6\u7406\u3067\
  \u3059\u3002"
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
---

{{< edit_this_page >}}

## 何となぜ？
数値を丸めるとは、最も近い整数または指定された小数点以下で調整することを意味します。これはプログラミングにおいて複雑さを減らし、パフォーマンスを向上させ、特定のポイントを超える精度が価値を加えない場合に行う標準的な処理です。

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
