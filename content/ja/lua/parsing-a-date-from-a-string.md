---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:37:23.005971-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"

category:             "Lua"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? - 何となぜ？
日付を文字列から解析することは、文字列データの中にある日付情報を取り出し、利用可能な形式に変換することです。なぜプログラマーがこれを行うかというと、ユーザー入力やデータストリームなど、さまざまなソースから得た日付文字列を日付型オブジェクトに変換して、検索、計算、整形が必要だからです。

## How to: - どうやって：
```Lua
-- 日付文字列からオブジェクトを生成するサンプル
local date_string = "2023-04-12T08:30:00"

-- 標準的な文字列関数を使って分割
local pattern = "(%d+)-(%d+)-(%d+)T(%d+):(%d+):(%d+)"
local year, month, day, hour, minute, second = date_string:match(pattern)

-- 取得した数字文字列を整数に変換して日付テーブルを作成
local date_table = {
    year = tonumber(year),
    month = tonumber(month),
    day = tonumber(day),
    hour = tonumber(hour),
    minute = tonumber(minute),
    second = tonumber(second)
}

-- 確認用の出力
print(os.date("%Y-%m-%d %H:%M:%S", os.time(date_table)))
```

出力サンプル:
```
2023-04-12 08:30:00
```

## Deep Dive - 掘り下げ：
日付の文字列解析は、初期のコンピューティングからあり、データ交換の基本です。Luaでは、os.dateやos.time関数を使って日付データを操作しますが、組み込みの直接的なパース機能はありません。そのためには、パターンマッチング（正規表現に似た）機能を使うか、外部ライブラリを使用します。`string.match`を使った方法は、Lua標準の手法ですが、より複雑な日付フォーマットに対応するには、`lpeg`や`date`（LuaRocks経由で利用可能）などのライブラリが役立ちます。

## See Also - 関連情報：
- Lua 5.4リファレンスマニュアル：https://www.lua.org/manual/5.4/
- LuaRocksの`date`ライブラリ：https://luarocks.org/modules/tieske/date
- LPegライブラリ：http://www.inf.puc-rio.br/~roberto/lpeg/
- Wikipediaの「正規表現」：https://ja.wikipedia.org/wiki/%E6%AD%A3%E8%A6%8F%E8%A1%A8%E7%8F%BE
