---
date: 2024-01-26 03:41:46.065448-07:00
description: ''
lastmod: '2024-04-05T22:50:56.200120-06:00'
model: gpt-4-0125-preview
summary: "Lua\u306E\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u306F\u3001\
  \u5168\u4F53\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u30A4\u30F3\u30DD\u30FC\u30C8\
  \u3059\u308B\u3053\u3068\u306A\u304F\u3001regex-lite\u306E\u4F53\u9A13\u306E\u8FEB\
  \u529B\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u30AD\u30E3\u30EC\u30C3\u30C8\uFF08\
  `^`\uFF09\u3068\u30C9\u30EB\u8A18\u53F7\uFF08`$`\uFF09\u306F\u3001\u305D\u308C\u305E\
  \u308C\u6587\u5B57\u5217\u306E\u958B\u59CB\u3068\u7D42\u4E86\u306B\u30DE\u30C3\u30C1\
  \u3057\u307E\u3059\u3002`%p`\u306F\u4EFB\u610F\u306E\u53E5\u8AAD\u70B9\u6587\u5B57\
  \u306B\u30DE\u30C3\u30C1\u3057\u307E\u3059\u3002\u5148\u982D\u3068\u672B\u5C3E\u306E\
  \u53E5\u8AAD\u70B9\u3092\u632F\u308A\u6255\u3063\u305F\u5F8C\u3001`(.*),`\u3067\u4ED6\
  \u306E\u3059\u3079\u3066\u3092\u30AD\u30E3\u30D7\u30C1\u30E3\u3057\u3001\u305D\u306E\
  \u30AD\u30E3\u30D7\u30C1\u30E3\u30B0\u30EB\u30FC\u30D7\u3092\u4F7F\u7528\u3057\u3066\
  \u5168\u4F53\u306E\u30DE\u30C3\u30C1\u3092`\"%1\"`\u3067\u7F6E\u63DB\u3057\u307E\
  \u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

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
