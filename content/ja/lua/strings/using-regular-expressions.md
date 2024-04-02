---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:43.499909-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u6B63\
  \u898F\u8868\u73FE\u306F\u3001\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u306B\u57FA\
  \u3065\u3044\u3066\u6587\u5B57\u5217\u306E\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\
  \u30F3\u30B0\u304A\u3088\u3073\u64CD\u4F5C\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\
  \u3002\u6587\u5B57\u5217\u64CD\u4F5C\u306E\u8907\u96D1\u306A\u64CD\u4F5C\u3092\u52B9\
  \u7387\u7684\u306B\u6271\u3046\u3053\u3068\u304C\u3067\u304D\u308B\u67D4\u8EDF\u6027\
  \u3068\u52B9\u7387\u6027\u306E\u305F\u3081\u306B\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u691C\u8A3C\u3001\u691C\u7D22\u3001\u30C6\u30AD\u30B9\u30C8\u64CD\u4F5C\
  \u306A\u3069\u306E\u30BF\u30B9\u30AF\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\
  \u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.291079-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u6B63\
  \u898F\u8868\u73FE\u306F\u3001\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u306B\u57FA\
  \u3065\u3044\u3066\u6587\u5B57\u5217\u306E\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\
  \u30F3\u30B0\u304A\u3088\u3073\u64CD\u4F5C\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\
  \u3002\u6587\u5B57\u5217\u64CD\u4F5C\u306E\u8907\u96D1\u306A\u64CD\u4F5C\u3092\u52B9\
  \u7387\u7684\u306B\u6271\u3046\u3053\u3068\u304C\u3067\u304D\u308B\u67D4\u8EDF\u6027\
  \u3068\u52B9\u7387\u6027\u306E\u305F\u3081\u306B\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u691C\u8A3C\u3001\u691C\u7D22\u3001\u30C6\u30AD\u30B9\u30C8\u64CD\u4F5C\
  \u306A\u3069\u306E\u30BF\u30B9\u30AF\u306B\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\
  \u307E\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 何となぜ？

プログラミングにおける正規表現は、特定のパターンに基づいて文字列のパターンマッチングおよび操作を可能にします。文字列操作の複雑な操作を効率的に扱うことができる柔軟性と効率性のために、プログラマーは検証、検索、テキスト操作などのタスクにそれらを使用します。

## 方法

LuaはPerlやPythonといった言語のようにネイティブに正規表現をサポートしていません。代わりに、多くの一般的な正規表現の使用例をカバーするパターンマッチング機能を提供しています。しかし、完全な正規表現サポートには、`lrexlib`のようなサードパーティ製のライブラリを使用できます。

### Luaにおける基本的なパターンマッチング：

Luaはシンプルな置換や検索に使用できる強力なパターンマッチングシステムを提供しています：

```lua
-- シンプルな検索
local str = "Hello, World!"
if string.find(str, "World") then
  print("マッチが見つかりました！")
end
-- 出力: マッチが見つかりました！

-- シンプルな置換
local s = string.gsub("Lua は素晴らしい！", "素晴らしい", "素晴らしくすばらしい")
print(s)
-- 出力: Lua は素晴らしくすばらしい！
```

### 文字列の部分をキャプチャする：

パターンにマッチした文字列の部分をキャプチャすることができます：

```lua
local date = "今日は 17/05/2023です。"
local d, m, y = string.match(date, "(%d+)/(%d+)/(%d+)")
print("日:", d, "月:", m, "年:", y)
-- 出力: 日: 17 月: 05 年: 2023
```

### `lrexlib`を使って正規表現を使用する：

実際の正規表現を使用するには、`lrexlib`をインストールして使用できます。それをインストールしていると仮定します（`luarocks install lrexlib-pcre`）、より複雑なパターンマッチングを行うことができます：

```lua
local rex = require 'rex_pcre'

local text = "スペインの雨は主に平野に降ります。"
local regex = "\\bS\\w+"
local count, err = rex.gsub(text, regex, function(w)
  return w:upper()
end)
if err then
  print("エラー:", err)
else
  print("変更されたテキスト:", text)
  print("置換された回数:", count)
end
-- 例の出力: 変更されたテキスト: スペインの雨は主に平野に降ります。
-- 置換された回数: 3
```

上記の例は、Lua自体のパターンマッチングシステム内での基本的な使用方法と、`lrexlib`を介して正規表現の力を利用する方法を示しています。シンプルな文字列操作を実行する場合も、正規表現の全ての柔軟性を必要とする場合も、強力なライブラリと組み合わせたLuaは、あなたのニーズに応えることができます。
