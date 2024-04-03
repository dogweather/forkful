---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:00.543955-07:00
description: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B\u3068\u306F\
  \u3001\u6587\u4E2D\u306E\u5404\u5358\u8A9E\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\
  \u5927\u6587\u5B57\u306B\u3057\u3001\u6B8B\u308A\u3092\u5C0F\u6587\u5B57\u306B\u3059\
  \u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u306E\u6280\u8853\
  \u306F\u3001\u30BF\u30A4\u30C8\u30EB\u306E\u6E96\u5099\u3084\u30E6\u30FC\u30B6\u30FC\
  \u5165\u529B\u3092\u8868\u793A\u7528\u306B\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3059\
  \u308B\u306A\u3069\u3001\u3088\u308A\u30D7\u30ED\u30D5\u30A7\u30C3\u30B7\u30E7\u30CA\
  \u30EB\u307E\u305F\u306F\u53EF\u8AAD\u6027\u306E\u9AD8\u3044\u51FA\u529B\u306E\u305F\
  \u3081\u306B\u4E00\u822C\u7684\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.280612-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B\u3068\u306F\
  \u3001\u6587\u4E2D\u306E\u5404\u5358\u8A9E\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\
  \u5927\u6587\u5B57\u306B\u3057\u3001\u6B8B\u308A\u3092\u5C0F\u6587\u5B57\u306B\u3059\
  \u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u3053\u306E\u6280\u8853\
  \u306F\u3001\u30BF\u30A4\u30C8\u30EB\u306E\u6E96\u5099\u3084\u30E6\u30FC\u30B6\u30FC\
  \u5165\u529B\u3092\u8868\u793A\u7528\u306B\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3059\
  \u308B\u306A\u3069\u3001\u3088\u308A\u30D7\u30ED\u30D5\u30A7\u30C3\u30B7\u30E7\u30CA\
  \u30EB\u307E\u305F\u306F\u53EF\u8AAD\u6027\u306E\u9AD8\u3044\u51FA\u529B\u306E\u305F\
  \u3081\u306B\u4E00\u822C\u7684\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 方法：
Luaには文字列を大文字にするための組み込み関数はありませんが、基本的な文字列操作関数を使用してこのタスクを簡単に実行できます。ここに、単一の単語の最初の文字を大文字にする簡単な関数があります：

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- 出力: Hello
```

文中の各単語を大文字にするには、文を単語に分割し、それぞれを大文字にしてから再結合します：

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- 出力: Hello World From Lua
```

もし、パフォーマンスが重要なプロジェクトに取り組んでおり、より高度な文字列操作機能が必要になった場合は、`Penlight`のようなサードパーティライブラリの使用を検討してください。Penlightは、Luaに他のユーティリティとともに、より多様な文字列処理機能を強化します：

```lua
-- Penlightがインストールされていると仮定：
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- 出力: Hello lua users

-- 注意：Penlightのcapitalized関数は最初の単語のみを大文字にします。
-- 各単語を大文字にするには、カスタムソリューションを実装するか、他のライブラリを探索することになります。
```
