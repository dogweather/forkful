---
title:                "日付を文字列に変換する"
html_title:           "Bash: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

日付を文字列に変換する理由は、日付をより見やすく表示したり、ファイル名やデータベースのクエリとして使用するためです。Bashには、簡単に日付を文字列に変換するための便利なコマンドがあります。

## How To

Bashでの日付の文字列への変換方法は、`date`コマンドを使用します。以下の例をご覧ください。

```
Bash - マイナス2

$ date +"今日は%Y年%m月%d日です"
今日は2021年12月31日です
```

このコマンドでは、`date`を実行して今日の日付を`%Y`（年）、`%m`（月）、`%d` （日）の形式で表示します。`+`の後に、表示したい文字列の形式を指定することもできます。例では、"今日は"の後に日付の情報が表示されます。

また、日付を文字列として表示する際には、`date`コマンドの`-d`オプションを使用することもできます。例を見てみましょう。

```
Bash - マイナス2

$ date -d "next Monday" +"来週の月曜日は%m月%d日です"
来週の月曜日は01月03日です
```

このように、`-d`オプションを使うと、"next Monday"（来週の月曜日）や"tomorrow"（明日）などの相対的な日付を指定することができます。そして、`+`の後に日付の文字列の形式を指定することで、指定した日付をその形式で表示することができます。

## Deep Dive

さまざまな場面で日付を文字列に変換する必要がある場合、`date`コマンドを利用することで便利に日付を操作することができます。しかし、Bashの詳細な日付操作を学ぶには、GNUの[公式ドキュメント](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)を参照することがおすすめです。

また、日付を文字列に変換する以外にも、`date`コマンドにはさまざまな機能が備わっています。例えば、ファイルのタイムスタンプを変更したり、日時を計算したりすることもできます。詳細は[参考リンク](https://www.howtogeek.com/449475/how-to-use-the-date-command-in-linux/)をご覧ください。

## See Also

参考リンク:
- [GNU Coreutils 公式ドキュメント](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)
- [日付操作についての詳細なガイド（英語）](https://www.howtogeek.com/449475/how-to-use-the-date-command-in-linux/)