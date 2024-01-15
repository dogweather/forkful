---
title:                "未来あるいは過去の日付を計算する"
html_title:           "Fish Shell: 未来あるいは過去の日付を計算する"
simple_title:         "未来あるいは過去の日付を計算する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##なぜ
未来や過去の日付を計算することに興味がある場合、あなたのコマンドラインでそれを行うことができます。例えば、誕生日や記念日を計算したり、派手なリマインダーを設定したりすることができます。それでは、Fish Shellでどのように日付を計算するかを見ていきましょう。

##方法
Fish Shellで日付を計算するには、次のようにコマンドを入力します。

```
date -d "1 day"
```

これは、明日の日付を表示するコマンドです。他の日付や時間の計算方法もあります。たとえば、次のように一週間後の日付を表示することもできます。

```
date -d "1 week"
```

また、過去の日付を計算することもできます。次のように、一ヶ月前の日付を表示することができます。

```
date -d "1 month ago"
```

詳細な日付の計算方法については、公式のドキュメントをチェックすることができます。また、高度な日付の計算については、`man`コマンドを使用してマニュアルを確認することもできます。

##ディープダイブ
日付を計算するためのディープダイブ（深い掘り下げ）をする前に、一般的なシェルコマンドの基礎を理解しておく必要があります。Fish Shellの公式サイトや他のリソースから簡単に見つけることができるので、それらをチェックするのも良いでしょう。また、コマンドラインの日付計算以外にも、Fish Shellには便利なコマンドや機能がたくさんあるので、ぜひ探索してみてください。

##参考リンク

- [Fish Shell公式サイト](https://fishshell.com/)
- [Fish Shellのドキュメント](https://fishshell.com/docs/current/index.html)
- `man`コマンドを使用してターミナル内でマニュアルを確認する方法を学ぶこともできます。