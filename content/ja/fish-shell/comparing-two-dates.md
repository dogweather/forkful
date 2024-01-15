---
title:                "「二つの日付を比較する」"
html_title:           "Fish Shell: 「二つの日付を比較する」"
simple_title:         "「二つの日付を比較する」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

二つの日付を比較することの利点を最大2文で説明します。

日付を比較することにより、特定の期間内のイベントを特定することができます。例えば、ある日付よりも後に行われたタスクを見つけたり、ある期間内に作成されたファイルを抽出することができます。

## 方法

Fish Shellを使用して、二つの日付を比較する方法を見ていきましょう。

まずはFish Shellのターミナルを開き、```date +%Y-%m-%d```コマンドを入力して今日の日付を確認します。

```
Fish Shell> date +%Y-%m-%d
2021-07-02
```

次に、比較したい日付を指定して、```set```コマンドで変数に代入します。

```
Fish Shell> set start_date 2021-06-01
Fish Shell> set end_date 2021-06-30
```

そして、```if```条件文を使用して、start_dateとend_dateを比較し、```echo```コマンドで結果を出力します。

```
Fish Shell> if test $start_date -gt $end_date; echo "start_dateはend_dateよりも遅いです"; else; echo "start_dateはend_dateよりも前です"; end
start_dateはend_dateよりも前です
```

以上のように、Fish Shellを使用することで簡単に日付を比較することができます。

## ディープダイブ

日付の比較には、通常Unixエポック時間が使用されます。Unixエポック時間とは、1970年1月1日からの経過秒数のことです。Fish Shellでは、Unixエポック時間を返す```date +%s```コマンドを使用することができます。

また、日付の比較をより高度にする方法として、タイムスタンプという概念もあります。タイムスタンプとは、毎秒インクリメントされる番号で、日付と時間の特定のポイントを表します。Fish Shellでは、タイムスタンプを返す```date +%s%N```コマンドを使用することができます。

さらに、日付の比較におけるタイムスタンプの重要な役割として、データベースやログファイルの同期が挙げられます。タイムスタンプを使用することで、正確なタイミングでデータを取得することができます。

## さらに読む

ここまで日付の比較について見てきましたが、Fish Shellには他にも便利なコマンドや機能がたくさんあります。以下のリンクを参考に、より詳細な情報をご覧ください。

- [Fish Shell Documentation](https://fishshell.com/docs/current/)
- [Fish Shell Cheatsheet](https://github.com/fisherman/fisher/blob/master/docs/fish_shell_cheat_sheet.md)
- [Fish Shell Tips and Tricks](https://medium.com/@JudeOsborn/fish-shell-tips-and-tricks-3154a1c2da7)