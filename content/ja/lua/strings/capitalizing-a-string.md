---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:00.543955-07:00
description: "\u65B9\u6CD5\uFF1A Lua\u306B\u306F\u6587\u5B57\u5217\u3092\u5927\u6587\
  \u5B57\u306B\u3059\u308B\u305F\u3081\u306E\u7D44\u307F\u8FBC\u307F\u95A2\u6570\u306F\
  \u3042\u308A\u307E\u305B\u3093\u304C\u3001\u57FA\u672C\u7684\u306A\u6587\u5B57\u5217\
  \u64CD\u4F5C\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u3053\u306E\u30BF\u30B9\u30AF\
  \u3092\u7C21\u5358\u306B\u5B9F\u884C\u3067\u304D\u307E\u3059\u3002\u3053\u3053\u306B\
  \u3001\u5358\u4E00\u306E\u5358\u8A9E\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\
  \u6587\u5B57\u306B\u3059\u308B\u7C21\u5358\u306A\u95A2\u6570\u304C\u3042\u308A\u307E\
  \u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.131629-06:00'
model: gpt-4-0125-preview
summary: ''
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
