---
title:                "正規表現を使う"
html_title:           "PowerShell: 正規表現を使う"
simple_title:         "正規表現を使う"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何して:
プログラミングの世界では、正規表現を使っています。正規表現とは、テキストのパターンを一致させることができる機能です。プログラマーは、特定の文字列やパターンを検索したり、置換したりするために正規表現を使用します。

## 方法:

```PowerShell
# テキストの中からパターンを検索する例
$Text = "こんにちは、今日は良い天気ですね。"
$Pattern = "今日"

if ($Text -match $Pattern) {
    Write-Host "パターンが見つかりました：" $Matches[0]
    # パターンが見つかりました：今日
}
```

```PowerShell
# テキストを置換する例
$Text = "私の電話番号は070-1234-5678です。"
$Pattern = "\d{3}-\d{4}-\d{4}"
$Replacement = "XXX-XXXX-XXXX"

$NewText = $Text -replace $Pattern, $Replacement
Write-Host "置換後のテキスト：" $NewText
# 置換後のテキスト：私の電話番号はXXX-XXXX-XXXXです。
```

## 詳細情報:
正規表現は、1960年代に誕生したテキスト処理のための強力なツールです。Perl言語で最初に実装されましたが、今ではほとんどのプログラミング言語でサポートされています。他のオプションとして、PowerShellでは「Like」演算子を使ってパターンマッチングができますが、正規表現はより柔軟な方法でパターンを一致させることができます。正規表現は、文法やメタキャラクターなどの専門的な概念を覚える必要がありますが、慣れると便利なツールとなります。

## 関連リンク:
- 正規表現入門(Powershell版): http://www-creators.com/archives/1658
- PowerShellの置換と正規表現: https://powershell.keicode.com/script/alphabet-matching-string-with-regex.php