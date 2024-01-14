---
title:    "Ruby: 日付を文字列に変換する"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換する理由は、プログラム内で日付を扱う必要があるためです。日付を文字列に変換することで、データベースに保存したり、ユーザーに表示したりすることができます。

## 方法

Rubyでは、日付を`strftime`メソッドを使って任意のフォーマットに変換することができます。例えば、以下のように書きます。

```Ruby
date = Date.today
date.strftime("%Y年%m月%d日")
```

このコードでは、今日の日付をフォーマット`年-月-日`に変換して表示します。出力結果は`2020年07月22日`となります。

## 深堀り

`strftime`メソッドの引数には、フォーマットを指定する文字列を渡します。たとえば、`%Y`は4桁の西暦、`%m`は2桁の月、`%d`は2桁の日の数字を表します。詳しいフォーマットの一覧は公式ドキュメント[^1]を参照してください。

また、`strptime`メソッドを使うことで、文字列を日付オブジェクトに変換することもできます。例えば、以下のように書きます。

```Ruby
date_str = "2020-07-22"
Date.strptime(date_str, "%Y-%m-%d")
```

このコードでは、文字列`2020-07-22`をフォーマット`年-月-日`に従って日付オブジェクトに変換します。

## 参考リンク

* [Ruby Core Reference: Date モジュール][^1]
* [Rubyドキュメント: strftime と strptime][^2]

[^1]: https://docs.ruby-lang.org/ja/latest/class/Date.html
[^2]: https://docs.ruby-lang.org/ja/latest/method/Date/i/strftime.html