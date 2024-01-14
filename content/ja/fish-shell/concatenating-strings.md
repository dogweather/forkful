---
title:                "Fish Shell: 「文字列の結合」"
simple_title:         "「文字列の結合」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ？
文字列を連結することの利点は何でしょうか？友達とのメッセージを結合すると、長いメッセージを短くまとめることができたり、メッセージの整形ができたりするため、より効率的なコミュニケーションが可能になります。

## 方法
まずは簡単な例から始めましょう。Fish Shellは、その簡潔さと柔軟性から多くの人々に利用されています。今回は、２つの文字列を結合する方法を紹介します。

```Fish Shell
set message 'こんにちは'
echo $message', 私の名前はHiroです。'
```
出力：
```
こんにちは, 私の名前はHiroです。
```

上の例では、まず`set`コマンドを使用して、`message`という変数に「こんにちは」という文字列を割り当てます。そして、`echo`コマンドを使用して`$message`という変数と「私の名前はHiroです。」という文字列を連結して出力します。

また、Fish Shellでは**command substitution**という機能を使用することで、コマンドの出力結果を変数に割り当てることができます。例えば、`date`コマンドの出力結果を変数に割り当てて、文字列と一緒に連結することができます。

```Fish Shell
set date (date +%Y-%m-%d)
echo '今日の日付は'$date'です。'
```
出力：
```
今日の日付は2021-05-05です。
```

## 深堀り
Fish Shellでは、文字列を結合する際に**=`(equals sign)**を使用することで、より簡潔にコードを書くことができます。上の例を`set`コマンドではなく、`=$`を使用して書き換えると以下のようになります。

```Fish Shell
set message = 'こんにちは'
echo $message', 私の名前はHiroです。'
```
出力：
```
こんにちは, 私の名前はHiroです。
```

また、変数を連結する際に`&`を使用することもできます。この方法を使用すると、変数の値が並べて表示されます。

```Fish Shell
set message1 = 'こんにちは'
set message2 = ', 私の名前はHiroです。'
echo $message1&$message2
```
出力：
```
こんにちは, 私の名前はHiroです。
```

## See Also（参考）
- [Fish Shellのドキュメンテーション](https://fishshell.com/docs/current/cmds/set.html)
- [文字列を扱う際の基本的なコマンド](https://fishshell.com/docs/current/tutorial.html#tut_string)
- [より高度な文字列操作についてのガイド](https://www.mankier.com/7/fish_expansion)