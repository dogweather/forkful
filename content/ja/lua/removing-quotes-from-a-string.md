---
title:                "文字列から引用符を削除する"
date:                  2024-01-26T03:41:46.065448-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"

category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 何となく？なぜ？
文字列から引用符を剥がすことは、テキストを抱擁しているそれらの二重引用符または単一引用符を剥ぎ取ることを意味します。コーダーはこれを行い、入力のサニタイズ、解析の容易さ、または一貫性のない引用で可能性があるデータを調和させるためです。

## 方法：
Luaで引用符を追い出す方法はこちらです：

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Hello, World!"'))     -- Hello, World!
print(remove_quotes("'Goodbye, Quotes!'"))  -- Goodbye, Quotes!
```

ビンゴ！それらの引用符は、洗濯機の靴下みたいに消えました。

## 詳細分析
人々は言語がテキストを扱えるようになってからずっと、文字列から引用符を削除してきました。Luaでは、`gsub`関数が重労働をし、パターンを精密なメスのように使って引用符を除去します。代替手段？もちろん、サポートされている言語で正規表現（regex）に行くこともできますし、各文字を通して噛み砕く自分自身のループを書くこともできます（あくびですが、ええ、あなたの時間です）。

Luaのパターンマッチングは、全体のライブラリをインポートすることなく、regex-liteの体験の迫力を提供します。キャレット（`^`）とドル記号（`$`）は、それぞれ文字列の開始と終了にマッチします。`%p`は任意の句読点文字にマッチします。先頭と末尾の句読点を振り払った後、`(.*),`で他のすべてをキャプチャし、そのキャプチャグループを使用して全体のマッチを`"%1"`で置換します。

Luaのパターンマッチングが完全な正規表現エンジンほど強力ではないことを覚えておいてください - たとえば、カウントやバックトラックができません。この単純さは、どの引用符を捌いていて、それらがどこに隠れているかによって、祝福でもあり、呪いでもあります。

## 参照
Luaのパターンマッチングにさらに深く飛び込むには、PiL（Programming in Lua）本をご覧ください：http://www.lua.org/pil/20.2.html

純粋なエレガンスのために、Pythonの`str.strip`で始まる他の言語の対応方法を比較してみてください：https://docs.python.org/3/library/stdtypes.html#str.strip
