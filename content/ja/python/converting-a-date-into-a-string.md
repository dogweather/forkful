---
title:                "Python: 日付を文字に変換する"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングをする際、日付を文字列に変換する理由は様々あります。例えば、データベースに日付を保存する際や、ユーザーにとって分かりやすい形式で日付を表示する必要がある場合などです。

## 方法

日付を文字列に変換する方法はいくつかありますが、今回はPythonの`strftime()`関数を使用してみましょう。以下のようにコードを書くことで、日付を任意のフォーマットの文字列に変換することができます。

```Python
import datetime

date = datetime.date(2021, 5, 25) # 変換したい日付を指定
string_date = date.strftime("%Y年%m月%d日") # 変換したいフォーマットを指定
print(string_date) # 出力：2021年05月25日
```

`strftime()`関数では`%Y`、`%m`、`%d`などの指定子を使用して、日付を年月日などの指定した形式に変換することができます。詳しくはPythonの公式ドキュメントを参考にしてください。

## ディープダイブ

日付を文字列に変換する際、`strftime()`関数以外にも別の方法があります。例えば、`datetime`モジュールの`strftime()`関数を使用せずに、日付を文字列に変換することも可能です。また、異なる言語やフレームワークでも同様の機能を持つ関数が存在する場合があります。プログラムを書く際には、自分が使用している言語や環境でどのように日付を扱えるか調べることが大切です。

## See Also

- [Python 公式ドキュメント - strftime()関数](https://docs.python.org/ja/3/library/datetime.html#strftime-and-strptime-format-codes)
- [Pythonの日付を文字列に変換する方法 - TechAcademyマガジン](https://techacademy.jp/magazine/18467)
- [日付のフォーマットを指定して文字列に変換する - Qiita](https://qiita.com/ikentooo/items/a3418447dee676aa569c)