---
title:                "「現在の日付の取得」"
html_title:           "Bash: 「現在の日付の取得」"
simple_title:         "「現在の日付の取得」"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
Bashには、現在の日付を取得するための便利なコマンドがあります。シェルスクリプトを書いているときや、ターミナルで日付を確認したいときに便利です。

## 使い方
```Bash
date
```
このコマンドを実行すると、現在の日付が表示されます。デフォルトでは、年月日の順に表示されますが、オプションを使って日付のフォーマットを指定することもできます。例えば、```date '+%a, %b %d %Y'```と入力すると、曜日、月、日、年の順に日付が表示されます。

```Bash
date '+%R'
```
上記のように入力すると、現在の時刻を24時間制で表示することもできます。

## 詳細を掘り下げる
日付を取得するためにBashが使用するのは、Linuxユーティリティの```date```コマンドです。このコマンドには様々なオプションがあり、簡単にカスタマイズすることができます。例えば、```date --help```と入力すると、利用可能なオプションの一覧が表示されます。

また、日付だけではなく、時刻やタイムゾーンなど、さまざまな情報を取得することもできます。もしもっと詳細を知りたい場合は、```man date```と入力してマニュアルを確認することもできます。

## 参考リンク
- [Linuxコマンドの基礎：日付や時刻を扱う「date」コマンドの使い方](https://knowledge.sakura.ad.jp/7736/)
- [Bashの一般的なコマンド一覧](https://qiita.com/magicant/items/b3b6395f0d45f0ce5f3d)