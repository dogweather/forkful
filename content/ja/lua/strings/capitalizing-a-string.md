---
title:                "文字列を大文字にする"
aliases:
- /ja/lua/capitalizing-a-string/
date:                  2024-02-03T19:06:00.543955-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何を、なぜ？
文字列を大文字にするとは、文中の各単語の最初の文字を大文字にし、残りを小文字にすることを意味します。この技術は、タイトルの準備やユーザー入力を表示用にフォーマットするなど、よりプロフェッショナルまたは可読性の高い出力のために一般的に使用されます。

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
