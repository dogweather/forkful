---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何となぜ？
日付比較のプログラミングは、二つの日付を比較して大小関係を判定することです。プログラマーがこれをする理由は、例えばイベントが予定通り進行中か、データの有効期限が切れたかなどを判定するためです。

## 実際にやってみよう：
以下に日付を比較するBashプログラミングのサンプルを示します：

```Bash
#!/bin/bash

# Date in the format (yyyy-mm-dd)
date1='2022-09-15'
date2='2022-09-16'

if [[ "$date1" > "$date2" ]]
then
    echo "$date1 is after $date2"
elif [[ "$date1" < "$date2" ]]
then
    echo "$date1 is before $date2"
else
    echo "$date1 and $date2 are same"
fi
```
このスクリプトの出力は、「2022-09-15 is before 2022-09-16」になります。

## ディープダイブ：
### 歴史的な背景
Bashは1979年に登場したsh（Bourne Shell）の拡張版です。日付比較の方法は頻繁に更新されてきましたが、現在では上記のコードが広く使用されています。

### 他のオプション
比較操作については他のシェルスクリプト言語（sh, ksh, zsh等）でも同様の機能が存在します。他にも、PHPやPythonなどのスクリプト言語でも日付比較が可能です。

### 実装詳細
上記スクリプトは文字列をそのまま比較しています。しかし、Unix タイムスタンプ（1970年1月1日からの経過秒数）を使用して日付比較を行うことも可能です。

## 参考文献：
1. Bashシェルスクリプトの基本: https://learnxinyminutes.com/docs/bash/
2. Bashでの日付操作: https://www.cyberciti.biz/faq/howto-compare-two-dates/
3. Unixタイムスタンプの活用: https://www.unixtimestamp.com/