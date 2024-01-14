---
title:                "Ruby: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換する理由には、様々なものがあります。例えば、データベースに日付を保存する際に文字列形式で保存する必要があったり、特定の日付フォーマットに合わせる必要があったりする場合があります。

## 方法

Rubyでは、`strftime`メソッドが日付を文字列に変換するためによく使われます。以下のように使用することができます。

```Ruby
# 現在の日付を取得
date = Date.today

# 任意のフォーマットで文字列に変換
date.strftime("%Y年%m月%d日")
# => "2020年08月02日"

# 曜日を含むフォーマットに変換
date.strftime("%Y年%m月%d日 %A")
# => "2020年08月02日 日曜日"
```

`strftime`メソッドでは、様々なフォーマットを指定することができます。詳しくは公式ドキュメントを参照してください。

## 深堀り

`strftime`メソッドでは、日付や時間に関する情報を引数として指定することで、より詳細なフォーマットを実現することができます。例えば、以下のようなものがあります。

- `%Y` : 年（4桁）
- `%m` : 月（2桁）
- `%d` : 日（2桁）
- `%H` : 時間（24時間制、2桁）
- `%M` : 分（2桁）
- `%S` : 秒（2桁）
- `%w` : 曜日（数字）
- `%B` : 月の名前（英語）
- `%j` : 年間通算日数（3桁）

これらの情報を組み合わせて、自分の必要なフォーマットを作ることができます。

## 参考リンク

- [strftimeメソッドのドキュメント](https://ruby-doc.org/core-2.7.0/Time.html#method-i-strftime)
- [Rubyの日付を文字列に変換する方法](https://www.delftstack.com/ja/howto/ruby/how-to-convert-date-to-string-in-ruby/)
- [プログラミング初心者が覚えたい RubyのDateとTimeの操作方法](https://qiita.com/potfish/items/667f0387cd0b6f8e2f88) 

## 関連リンク