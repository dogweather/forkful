---
date: 2024-01-20 17:43:26.083372-07:00
description: "\u6587\u5B57\uFF08\u3082\u3058\uFF09\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\
  \u81F4\uFF08\u3044\u3063\u3061\uFF09\u3059\u308B\u3082\u306E\u3092\u524A\u9664\uFF08\
  \u3055\u304F\u3058\u3087\uFF09\u3059\u308B\u306E\u306F\u3001\u4E0D\u8981\u306A\u6587\
  \u5B57\u3092\u53D6\u308A\u9664\u304F\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u3092\u304D\u308C\u3044\u306B\u3059\
  \u308B\u305F\u3081\u3001\u3042\u308B\u3044\u306F\u5165\u529B\u30C7\u30FC\u30BF\u3092\
  \u51E6\u7406\u3059\u308B\u969B\u306B\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.846788-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\uFF08\u3082\u3058\uFF09\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\
  \u81F4\uFF08\u3044\u3063\u3061\uFF09\u3059\u308B\u3082\u306E\u3092\u524A\u9664\uFF08\
  \u3055\u304F\u3058\u3087\uFF09\u3059\u308B\u306E\u306F\u3001\u4E0D\u8981\u306A\u6587\
  \u5B57\u3092\u53D6\u308A\u9664\u304F\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u3092\u304D\u308C\u3044\u306B\u3059\
  \u308B\u305F\u3081\u3001\u3042\u308B\u3044\u306F\u5165\u529B\u30C7\u30FC\u30BF\u3092\
  \u51E6\u7406\u3059\u308B\u969B\u306B\u884C\u3044\u307E\u3059\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ?)
文字（もじ）パターンに一致（いっち）するものを削除（さくじょ）するのは、不要な文字を取り除くことです。プログラマーは、データをきれいにするため、あるいは入力データを処理する際に行います。

## How to: (やり方) 
```Lua
local text = "こんにちは、プログラミングの世界へ！123"
local pattern_to_remove = "%D"  -- 非数字 (ひすうじ) のパターン

-- gsub関数でパターンに一致する文字を削除
local cleaned_text = string.gsub(text, pattern_to_remove, "")
print(cleaned_text)  -- 出力: 123
```
上のコードでは、`%D`を使って非数字を削除しています。

```Lua
local text = "フィルター後のテキスト: あいうえお123456"
local pattern_to_remove = "[あいうえお]"  -- 特定の文字 (とくていのもじ) のパターン

-- gsub関数で特定の文字を削除
local filtered_text = string.gsub(text, pattern_to_remove, "")
print(filtered_text)  -- 出力: フィルター後のテキスト: 123456
```
この例では、`[あいうえお]`のパターンを使って特定の文字を削除しています。

## Deep Dive (詳細な解説)
パターンマッチというのは、Luaがバージョン5.1から実装した機能です。`string.gsub`関数は最もよく使われます。パターンは正規表現（せいきひょうげん）に似ていますが、Luaのパターンマッチングは正規表現よりもシンプルです。パターンを使って、選択した種類の文字を一括で削除できます。

代替方法として、文字列の反復処理と条件分岐を使う方法もありますが、効率が悪くなることがあります。また、`string.find` や `string.match` を使って特定のパターンに一致する部分だけを探し出してから削除することもできますが、`string.gsub` は一回の呼び出しで処理が完了するため便利です。

## See Also (関連情報)
- Lua 5.4 Reference Manual (英語): [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
- Lua パターンマッチングについての解説 (日本語): [https://www.lua.org/pil/20.2.html](https://www.lua.org/pil/20.2.html)
