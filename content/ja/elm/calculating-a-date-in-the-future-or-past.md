---
title:                "未来または過去の日付の計算"
html_title:           "Elm: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

＃＃何か＆なぜ？
将来の日付や過去の日付を計算するとは、ある日付から指定された日数だけ前や後の日付を求めることです。 プログラマーがこれを行うのは、例えばイベントの日程を自動で計算するなど、日付に関わる処理を効率的に行うためです。

＃＃方法：
```Elm
-- 将来の日付を計算する例
import Time

Time.add Time.Day 7 (Time.millisToPosix 1589841600000) -- 2020年5月19日から7日後の日付が計算される 

-- 過去の日付を計算する例
import Time

Time.sub Time.Week (-2) (Time.millisToPosix 1589841600000) -- 2020年5月19日から2週間前の日付が計算される
```

＃＃深堀り：
日付を計算する方法は、歴史的には太陽暦や暦の仕組みに関わる問題から生まれてきました。しかし、現在はプログラミング言語やライブラリによって効率的に計算することができます。また、便利なツールとしてJavaScriptの「Dateオブジェクト」やPythonの「datetimeモジュール」などがあります。

＃＃関連リンク：
- [Elm Timeモジュールドキュメント](https://package.elm-lang.org/packages/elm/time/latest/)
- [JavaScript Dateオブジェクトドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Python datetimeモジュールドキュメント](https://docs.python.org/ja/3/library/datetime.html)