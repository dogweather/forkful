---
title:    "Bash: 日付を文字列に変換する"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することについて、なぜ誰もがそれに取り組むのかを説明します。

## 方法

日付を文字列に変換するには、Bashの`$date`コマンドを使用します。

```Bash
date -d "2020-05-23" '+%Y年%m月%d日'
```

出力: 2020年05月23日

## 詳細について

日付を文字列に変換する際、`%Y`や`%m`などの特殊な文字を使用します。これらの文字は、日付の年、月、日を表します。Bashでは`date`コマンドで使うことができます。

例えば、`%Y`は4桁の年を表し、`%m`は2桁の月を表します。また、`%d`は2桁の日を表します。これらの特殊文字を使うことで、日付を任意の形式で表示することができます。

## 参考

同様のコマンドや日付の書式設定については以下のリンクを参考にしてください。

- [Bash `date`コマンドの使い方](https://www.atmarkit.co.jp/ait/articles/1901/14/news011.html)
- [日付を文字列に変換する方法 (英語)](https://bash.cyberciti.biz/guide/Converting_a_String_To_a_Date)
- [特殊文字の詳細について (英語)](https://www.tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-5.html#ss5.2) 

## 参考

[Bash `date`コマンドの使い方](https://www.atmarkit.co.jp/ait/articles/1901/14/news011.html)

[日付を文字列に変換する方法 (英語)](https://bash.cyberciti.biz/guide/Converting_a_String_To_a_Date)

[特殊文字の詳細について (英語)](https://www.tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-5.html#ss5.2)