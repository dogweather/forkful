---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:15:50.297297-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何と何故？)
現在の日付を取得することは、文字通りシステムの今の日付と時刻を得ることです。プログラマーはログのタイムスタンプ、有効期限、日付計算などにこれを使います。

## How to: (やり方)
```Lua
-- 現在の日付と時刻を取得
local current_time = os.date("*t")  -- テーブル形式で日付と時刻を取得

-- 様々なフォーマットで出力
print("今日の日付:", os.date("%Y-%m-%d"))  -- yyyy-mm-dd 形式
print("時刻:", os.date("%H:%M:%S"))  -- hh:mm:ss 形式
```

サンプル出力:
```
今日の日付: 2023-04-01
時刻: 15:42:07
```

## Deep Dive (探求)
Luaの`os.date`関数は、C言語の`strftime`関数に影響を受けています。パフォーマンスへの影響が最小限であるために、多くのLuaプログラムで使われています。`os.date("*t")`は、ローカル時間で日時のテーブルを返しますが、`os.date("!%X")`は世界協定時刻（UTC）を使います。

他の言語やフレームワークでは、日付取得に特専のライブラリやモジュールを提供することがありますが、Luaでは`os`モジュールがその役割を果たします。Luaを組み込んでいるアプリケーションでは、この機能がサンドボックス化されている場合があるので注意が必要です。

## See Also (参考文献)
- Lua 5.4 Reference Manual: https://www.lua.org/manual/5.4/manual.html#6.9
- strftime (C言語の時間関数): https://www.cplusplus.com/reference/ctime/strftime/
- Lua-users wiki (日付と時刻): http://lua-users.org/wiki/DateAndTime