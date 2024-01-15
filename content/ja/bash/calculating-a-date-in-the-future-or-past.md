---
title:                "「未来または過去の日付を計算する」"
html_title:           "Bash: 「未来または過去の日付を計算する」"
simple_title:         "「未来または過去の日付を計算する」"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ
日付を過去や未来に計算する必要があるかもしれません。例えば、予定を立てたり、特定の日付までの期間を計算するために利用します。

## 使い方
計算したい日付を"計算基準日"として設定し、その日付からどれだけ前後の日数を計算するか指定します。例えば、3日後の日付を計算したい場合は以下のように入力します。

```Bash
date -d "3 days"
```

または、過去の日付を計算したい場合はマイナス（-）をつけて指定します。例えば、10日前の日付を計算する場合は以下のように入力します。

```Bash
date -d "-10 days"
```

このように、"date"コマンドに"-d"オプションをつけて日付を計算することができます。

### 出力例
上記の入力例の場合、以下のように計算結果が表示されます。

```
Sat, 15 May 2021 00:00:00 +0900
```

このように、計算結果は日付のフォーマットで表示されます。

## ディープダイブ
実際には、"date"コマンドで計算することができるのは、グレゴリオ歴で1970年1月1日以降の日付だけです。しかし、その前の日付も計算することは可能です。その場合は、"--date"オプションを使います。例えば、1950年1月1日から3日後の日付を計算する場合は以下のように入力します。

```Bash
date -d "1950-01-01 + 3 days"
```

また、特定の日付までの期間を計算することもできます。例えば、今日から5日後までの期間を計算する場合は以下のように入力します。

```Bash
date -d "5 days"
```

このように、日付の計算は非常に簡単に行うことができますので、日常的なプログラミングにおいても便利な機能と言えます。

## 関連リンク

[GNUの公式ドキュメント](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html#date-invocation)

[UNIXコマンドについて-日付関連コマンド](https://www.itmedia.co.jp/help/tips/unix/017.html)

[日付を計算するためのコマンドの使い方【Bash,Linux,Shell】](https://mrotaru.hatenablog.com/entry/2018/08/10/080000)