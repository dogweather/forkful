---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:16:05.369845-07:00
simple_title:         "現在の日付を取得する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コードで現在の日付を取るってことは、単に今日の日付を指すんだ。プログラマはログ, 日付依存の機能, ユーザーのタイムゾーン表示などで使う。

## How to: (やり方)
Pythonでは、`datetime`モジュールで今の日付をサクッと取れる。こんな感じだ:

```Python
from datetime import date

# 現在の日付を取得する
today = date.today()

# 日付を文字列で表示する
print("今日の日付:", today)
```

実行結果:
```
今日の日付: 2023-04-12
```

タイムゾーンを考慮したいなら、`pytz`ライブラリも使える。

## Deep Dive (詳細情報)
`datetime`はPythonにおける日付と時刻を扱うための標準ライブラリだ。Python初期からあって、バージョンアップが続いている。`datetime.today()`などのメソッドでシステムのローカル日付と時刻を取得できるが、`datetime.now()`を使って`pytz`ライブラリでタイムゾーンを指定することもできる。

他の方法には、`time`モジュールや外部ライブラリの`arrow`や`delorean`などがある。これらは機能や使い心地に差があるから、プロジェクトの必要に応じて選ぶといい。

実装の詳細だが、`date.today()`は内部でコンピュータのシステムクロックを参照してる。だから、システムの日付と時刻設定が間違ってると、おかしい値が出るかもしれない。注意が必要だ。

## See Also (関連情報)
- Pythonの`datetime`モジュールのドキュメント: https://docs.python.org/3/library/datetime.html
- `pytz`ライブラリに関する情報: http://pytz.sourceforge.net/
- `time`モジュールについて: https://docs.python.org/3/library/time.html
- `arrow`ライブラリ: https://arrow.readthedocs.io/en/latest/
- `delorean`ライブラリ: https://delorean.readthedocs.io/en/latest/
