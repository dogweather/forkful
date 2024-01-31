---
title:                "数値の丸め処理"
date:                  2024-01-26T03:45:58.494627-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"

category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/rounding-numbers.md"
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
