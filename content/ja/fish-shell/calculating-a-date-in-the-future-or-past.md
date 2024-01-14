---
title:    "Fish Shell: 未来または過去の日付の計算"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を計算する必要性は、未来の予定を把握したり、過去の出来事を確認するために重要です。

## 方法

```Fish Shell```を使用して、未来または過去の日付を計算する簡単な方法があります。まず、```fish```コマンドでシェルを起動します。次に、```date```コマンドを使用して、今日の日付を取得します。

```
$ fish
$ date
Tue Dec 15 14:37:46 JST 2020
```

次に、```-d```オプションを使用して、未来または過去の日付を計算します。例えば、10日間後の日付を計算するには、以下のように入力します。

```
$ date -d "10 days"
Fri Dec 25 14:37:46 JST 2020
```

同様に、10日前の日付を計算するには、```ago```を追加します。

```
$ date -d "10 days ago"
Sat Dec 05 14:37:46 JST 2020
```

## ディープダイブ

日付を計算する際に、```date```コマンドに使用できる複数のオプションがあります。これらのオプションを使用することで、さまざまな形式の日付を計算することができます。また、```今日```や```昨日```といった相対的な表現を使用することもできます。詳細な情報については、オフィシャルドキュメントを参照してください。

## 参考リンク

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils: Date input formats](https://www.gnu.org/software/coreutils/manual/html_node/Date-input-formats.html#Date-input-formats)
- [How to calculate dates using the command line on Linux](https://opensource.com/article/20/4/calculating-dates-linux)