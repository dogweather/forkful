---
title:                "日付を文字列に変換する"
html_title:           "PowerShell: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なに？そしてなぜ？

日付を文字列に変換することは、プログラムでよく行われる処理です。プログラマーにとって、日付を文字列に変換することは、データの扱いや表示方法を柔軟にするために重要な手段です。

## 方法：

``` PowerShell
# 日付を文字列に変換する方法１
Get-Date -Format "yyyy/MM/dd"

# 出力例： 2021/01/15

# 日付を文字列に変換する方法２
(Get-Date).ToString("ddd MMM dd yyyy")

# 出力例： Fri Jan 15 2021 
```

## 詳細について

日付を文字列に変換する方法は、コンピューターのシステムに依存します。たとえば、Unixシステムでは、日付を秒単位の整数値として表現し、文字列に変換することで読みやすい日付形式に変換されます。また、Windowsシステムでは、日付を文字列に変換する際にフォーマットを指定することができます。

日付を文字列に変換する別の方法として、日付をテキストで表現する方法があります。たとえば、「2021年1月15日」という日付を、「2021/01/15」や「Fri Jan 15 2021」といった形式ではなく、「二〇二一年一月十五日」というテキストとして表現する方法です。

## 関連情報の参照：

- [PowerShell の入門](https://docs.microsoft.com/ja-jp/powershell/scripting/overview?view=powershell-7.1) 
- [日付と時刻の書式](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1#parameters)