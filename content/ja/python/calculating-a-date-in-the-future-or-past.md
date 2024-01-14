---
title:    "Python: 未来または過去の日付の計算"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ日付を計算するのか

日付を計算することは、将来や過去の日付を知るために非常に便利です。例えば、今日から1週間後の日付を計算することで、来週の予定を把握することができます。

## 方法

Pythonの「datetime」モジュールは、日付を計算するための便利なツールを提供しています。以下のコードを使うことで、指定した日数を加算・減算することができます:

```Python
import datetime

# 今日の日付を取得
today = datetime.date.today()

# 1週間後の日付を計算
one_week = today + datetime.timedelta(days=7)

# 1週間前の日付を計算
one_week_ago = today - datetime.timedelta(days=7)

print("今日:", today)
print("来週:", one_week)
print("先週:", one_week_ago)
```

出力結果は以下のとおりになります:

```
今日: 2021-05-01
来週: 2021-05-08
先週: 2021-04-24
```

## ディープダイブ

「datetime」モジュールの「timedelta」オブジェクトを使うことで、任意の日数を加算・減算することができます。また、日付の比較やフォーマットの変更など、日付の操作に関する様々な機能を提供しています。詳細な情報は、公式ドキュメントを参照してください。

## 参考リンク

- Python公式ドキュメント: https://docs.python.org/ja/3/library/datetime.html
- Pythonの日付操作について学ぶ: https://note.nkmk.me/python-datetime-timedelta/