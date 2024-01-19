---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

日付を文字列に変換することは何か？これはプログラムが日付Dataを文字列形式に変えるという手続きです。プログラマはなぜこれを行うのでしょうか？それは日付データを人間が読める形、または特定のフォーマットに従うためです。

## どうやって:

次のFish Shellコマンドは現在の日付を"YYYY-MM-DD"フォーマットの文字列に変換します。

```Fish Shell
date "+%Y-%m-%d"
```
これを実行すると、次のような出力が得られます:
```Fish Shell
2023-09-15
```

## ディープダイブ:

### 歴史的な背景:
言語ごとに日付を文字列に変換する方法は異なりますが、元々のUnix時間（1970年1月1日からの秒数）が日常生活の日付時間表現に変換される方法は同じです。

### 代替案:
Pythonなどの他のプログラミング言語では、次のように日付を文字列に変換することができます。

```Python
import datetime
print(datetime.date.today().strftime("%Y-%m-%d"))
```

### 実装の詳細:
Fish Shellでは、`date` コマンドを使って日付を文字列に変換します。"+%Y-%m-%d"は出力形式を制御します。

## 参考文献:

以下は関連するリソースへのリンクです:
1. [Fish Shell 公式文書](https://fishshell.com/docs/current/commands.html#date)
2. [Fish Shell スクリプトチュートリアル](https://fishshell.com/docs/current/tutorial.html)
3. [UNIX の date コマンド](http://man7.org/linux/man-pages/man1/date.1.html)
4. [Python 公式ドキュメンテーションDateオブジェクト](https://docs.python.org/ja/3/library/datetime.html)