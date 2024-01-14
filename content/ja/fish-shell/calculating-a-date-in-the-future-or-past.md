---
title:                "Fish Shell: 将来や過去の日付の計算。"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を計算するのはスケジュールを管理するためにとても重要です。未来の何日後や過去の日付を計算することで、予定を立てることができます。

## 作り方

Fish Shellの便利なコマンド「date -d」を使うことで、簡単に日付を計算することができます。まずは、計算したい日付を指定し、その後に計算したい日付のフォーマットを指定します。例えば、3日後の日付を計算するには、次のように入力します。

```Fish Shell
date -d '3 days'
```

このコマンドを実行すると、今日の日付から3日後の日付が表示されます。また、さまざまな日付フォーマットを指定することもできます。例えば、mm/dd/yyの形式で表示するには、次のように入力します。

```Fish Shell
date -d '3 days' '+%m/%d/%y'
```

このようにすることで、3日後の日付を「03/12/20」という形式で表示することができます。

## 深く掘り下げる

日付の計算にはさまざまなオプションがあり、これらを組み合わせることでより複雑な日付の計算を行うことができます。例えば、10日後の日付の月の最終日を計算するには、次のように入力します。

```Fish Shell
date -d '10 days' '1 month'
```

このコマンドを実行すると、月の最終日である「31」が表示されます。また、特定の日付から何日後や何日前の日付を計算することも可能です。例えば、2020年3月12日から200日後の日付を計算するには、次のように入力します。

```Fish Shell
date -d '200 days' '2020-3-12'
```

このコマンドを実行すると、2020年3月12日から200日後の日付である「9-28-20」が表示されます。

## 関連情報

もし日付を計算する際に困った時は、以下のリンクを参考にしてみてください。

- [Fish Shellチートシート](https://github.com/Nemoshk/fish-cheatsheet)
- [Fish Shell公式ドキュメント](https://fishshell.com/docs/current/cmds/date.html)

[to_md_toc]: en_US.markdown_toc