---
title:                "未来または過去の日付を計算する"
html_title:           "Bash: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何をし、なぜ？
"日付を将来または過去に計算する"とは、特定の日付から任意の日数を加算または減算して、新しい日付を算出することです。プログラマーは、このような計算を行うことで、日付に関連するデータや処理をより有効に扱うことができます。

## 方法：
下記のコードブロック内に、```Bash ... ``` の形式でコーディング例とサンプルの出力を記載します。

```Bash
# 現在の日付を取得
current_date=$(date +%Y-%m-%d)
echo "現在の日付は: ${current_date}"

# 10日後の日付を計算
future_date=$(date -d "+10 days" +%Y-%m-%d)
echo "10日後の日付は: ${future_date}"
```

出力：
```Bash
現在の日付は: 2021-07-22
10日後の日付は: 2021-08-01
```

## 深堀り：
(1) 歴史的背景：今日、日付の計算は標準的なプログラミングタスクとして考えられていますが、昔はそうではありませんでした。昔は手計算や専用の計算機が必要でしたが、現代のコンピューターの登場により簡単に計算することができるようになりました。

(2) 代替手段：Bashの`date`コマンドを使用する方法以外にも、PythonやJavaScriptなど別のプログラミング言語でも日付の計算を行うことができます。また、オンラインの日付計算ツールもあります。

(3) 実装の詳細：日付計算にはさまざまな方法がありますが、多くの場合はエポック秒を使用して計算を行います。エポック秒とは、UNIXの起点となる1970年1月1日の00:00:00からの経過秒数のことです。

## 関連情報：
- [Bashのdateコマンドの公式ドキュメント](https://ss64.com/bash/date.html)
- [Pythonでの日付計算の例](https://www.educative.io/edpresso/how-to-perform-date-and-time-calculation-in-python)
- [オンラインで日付計算を行うツール](https://www.timeanddate.com/date/dateadded.html)