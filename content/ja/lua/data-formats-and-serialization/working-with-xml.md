---
aliases:
- /ja/lua/working-with-xml/
date: 2024-01-26 04:34:20.367394-07:00
description: "XML\u3092\u4F7F\u7528\u3059\u308B\u4F5C\u696D\u306F\u3001\u30B3\u30FC\
  \u30C9\u3092\u7528\u3044\u3066XML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u306E\u89E3\
  \u6790\u3084\u64CD\u4F5C\u3092\u884C\u3046\u3053\u3068\u3092\u542B\u307F\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u4EA4\u63DB\
  \u3084\u4FDD\u5B58\u306B\u5E83\u304F\u4F7F\u7528\u3055\u308C\u3066\u3044\u308B\u69CB\
  \u9020\u5316\u3055\u308C\u305F\u643A\u5E2F\u53EF\u80FD\u306A\u5F62\u5F0F\u3067\u3001\
  \u30C7\u30FC\u30BF\u306E\u8AAD\u307F\u66F8\u304D\u3084\u5909\u66F4\u3092\u884C\u3046\
  \u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.056089
model: gpt-4-0125-preview
summary: "XML\u3092\u4F7F\u7528\u3059\u308B\u4F5C\u696D\u306F\u3001\u30B3\u30FC\u30C9\
  \u3092\u7528\u3044\u3066XML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u306E\u89E3\u6790\
  \u3084\u64CD\u4F5C\u3092\u884C\u3046\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u4EA4\u63DB\u3084\
  \u4FDD\u5B58\u306B\u5E83\u304F\u4F7F\u7528\u3055\u308C\u3066\u3044\u308B\u69CB\u9020\
  \u5316\u3055\u308C\u305F\u643A\u5E2F\u53EF\u80FD\u306A\u5F62\u5F0F\u3067\u3001\u30C7\
  \u30FC\u30BF\u306E\u8AAD\u307F\u66F8\u304D\u3084\u5909\u66F4\u3092\u884C\u3046\u305F\
  \u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "XML\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
XMLを使用する作業は、コードを用いてXMLドキュメントの解析や操作を行うことを含みます。プログラマーは、データ交換や保存に広く使用されている構造化された携帯可能な形式で、データの読み書きや変更を行うためにこれを行います。

## 方法：
LuaにはネイティブのXMLパーサーが含まれていませんが、LuaXMLやxml2luaのようなライブラリが存在し、仕事をこなします。xml2luaを使用してXMLを解析する方法を簡単に見てみましょう：

```Lua
local xml2lua = require("xml2lua")
local handler = require("xmlhandler.tree")

local xmlParser = xml2lua.parser(handler)
xmlParser:parse([[<root><book id="123">Programming in Lua</book></root>]])

print(handler.root.book._attr.id)  -- 出力： 123
print(handler.root.book[1])        -- 出力： Programming in Lua
```

XMLの書き込みについては、LuaXMLを使った小さな例をこちらに示します：

```Lua
local luaxml = require("LuaXML")

local xml = xml.new("root")
xml:append("book")[1] = "Programming in Lua"
xml.book._attr = {id="123"}

print(xml:tag())  -- 出力： <root><book id="123">Programming in Lua</book></root>
```

## 深く掘り下げて
XMLはExtensible Markup Languageの略で、90年代半ばからデータ表現と交換の標準となっています。XMLはデータに構造を与え、人が読むことができ、マシンが解析可能です。

JSONやYAMLはそのシンプルさから現在好まれていますが、XMLは多くの企業やレガシーシステムで依然として広く使用されています。LuaにはネイティブのXML処理が組み込まれていないのは、Luaが小さいサイズで拡張可能なモジュールを通じて設計されているためです。

LuaXML、xml2luaなどのLua用XMLライブラリは、このギャップを埋めます。LuaXMLは軽量なXMLリーダーとライターを提供し、xml2luaはSAXパーサーに似たイベント駆動のアプローチを使用します。これらのライブラリは通常、携帯性のために純粋なLuaで実装されていますが、一部はパフォーマンスのためにCに依存することがあります。

パフォーマンスとメモリ使用の面で、LuaのXMLライブラリはネイティブサポートを持つ言語のライブラリほど高速ではないかもしれません。しかし、Luaのほとんどの使用例、特にゲーム開発や組み込みシステムのスクリプトでは、これらのライブラリがシステムをオーバーロードすることなく十分な仕事をします。

## 参照
- GitHub上のLuaXML：https://github.com/LuaDist/luaxml
- GitHub上のxml2lua：https://github.com/manoelcampos/xml2lua
- Lua.orgのライブラリ一覧：https://lua-users.org/wiki/LibrariesAndBindings
