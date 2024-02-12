---
title:                "将来または過去の日付を計算する"
aliases:
- /ja/lua/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:50.995505-07:00
model:                 gpt-4-1106-preview
simple_title:         "将来または過去の日付を計算する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ？
計算上の未来または過去の日付って何？それは特定の日から一定の時間を加算または減算して、新しい日付を求めること。プログラマーはなぜこれをするの？周期的なイベント管理、予約システム、有効期限の追跡など、日付計算が必要なケースが多いからです。

## どうやって：
Luaでは、`os.date`と`os.time`関数を使って日付の計算ができます。例えば、今日から一週間後の日付を求めるには：

```lua
today = os.time()
one_week = 7 * 24 * 60 * 60  -- 一週間の秒数
future = os.date("*t", today + one_week)

print("現在の日付:", os.date("%Y-%m-%d"))
print("未来の日付:", os.date("%Y-%m-%d", os.time(future)))
```

出力例：
```
現在の日付: 2023-04-01
未来の日付: 2023-04-08
```

過去の日付を計算するには：

```lua
past = os.date("*t", today - one_week)
print("過去の日付:", os.date("%Y-%m-%d", os.time(past)))
```

出力例：
```
過去の日付: 2023-03-25
```

## 深掘り
Luaで日付計算に対する組み込みのサポートは限られています。`os.date`と`os.time`は基本ですが、複雑な日付操作には別のライブラリを使うことが一般的。かつては`LuaDate`や`date.lua`がよく使われました。しかし今では、よりモダンな`luatz`や`penlight`などがおすすめです。

Luaの内部で、時刻はエポックタイム（1970年1月1日からの秒数）で計算されるため、タイムゾーンに注意が必要。Luaはタイムゾーン情報を提供しないため、それを考慮する必要がある場合、外部ライブラリを利用するのがベストです。

時間計算で重要なのはうるう年やうるう秒の扱い。これらは、`os.date`や`os.time`を使う限り、自動的に考慮されます。

## 関連情報
- Luaの公式マニュアル: [https://www.lua.org/manual/](https://www.lua.org/manual/)
- `luatz` GitHubページ: [https://github.com/daurnimator/luatz](https://github.com/daurnimator/luatz)
- `penlight` GitHubページ: [https://github.com/stevedonovan/Penlight](https://github.com/stevedonovan/Penlight)
