---
title:                "将来または過去の日付の計算"
html_title:           "Python: 将来または過去の日付の計算"
simple_title:         "将来または過去の日付の計算"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なに & なぜ?
日付を未来や過去に計算することとは、指定された日付から一定の期間を増減させた新しい日付を生成することです。プログラマーがこれをする理由は、日付を操作したい場合や特定期間の計算を行いたい場合に役立つからです。

## 方法:
```Python
from datetime import date, timedelta 

# 今日の日付を取得 
today = date.today() 

# 一年後の日付を計算 
one_year_later = today + timedelta(days=365) 

print("今日の日付:", today)
print("一年後の日付:", one_year_later)
```

出力:
```
今日の日付: 2021-01-01 
一年後の日付: 2022-01-01
```

## ディープダイブ:
1. 歴史的な背景:
日付の計算が必要になった背景には、グレゴリオ暦の導入があります。グレゴリオ暦は、1582年にローマ教皇グレゴリウス13世によって採用されました。それまでのユリウス暦には誤差があったため、新しい暦が導入されることになりました。
2. 代替手段:
Pythonの標準モジュールであるdatetimeモジュールを使用する他、dateutilやarrowなどのサードパーティ製のモジュールもあります。
3. 実装の詳細:
Pythonでは、日付の計算を行うためにtimedeltaオブジェクトを使用します。これは、datetimeオブジェクトに対して日付の増減を行うことができるクラスです。

## 参考:
- datetimeモジュール: https://docs.python.org/ja/3/library/datetime.html
- dateutilモジュール: https://dateutil.readthedocs.io/en/stable/
- arrowモジュール: https://arrow.readthedocs.io/en/latest/