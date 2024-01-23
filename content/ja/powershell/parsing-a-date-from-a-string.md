---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:37:50.364660-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付の解析は文字列から日付データ型への変換です。プログラマーはデータ処理や日付の検証でよく使います。

## How to: (方法)
以下の例では、文字列から日付への解析方法を紹介します。

```PowerShell
# 基本的な日付の解析
$dateString = "2023-04-01"
$date = [datetime]::Parse($dateString)
echo $date

# 特定の文化に依存するフォーマットを指定
$culture = [Globalization.CultureInfo]::CreateSpecificCulture("ja-JP")
$date = [datetime]::ParseExact($dateString, "yyyy-MM-dd", $culture)
echo $date

# ParseExactを使ってフォーマットを厳格に適用
$formattedDate = [datetime]::ParseExact($dateString, "yyyy-MM-dd", $null)
echo $formattedDate
```

これらのコマンドを実行すると、適切な`DateTime`オブジェクトを得られます。

```PowerShell
Saturday, April 1, 2023 12:00:00 AM
Saturday, April 1, 2023 12:00:00 AM
Saturday, April 1, 2023 12:00:00 AM
```

## Deep Dive (深掘り)
日付解析は古くからありますが、PowerShellでは`.NET`クラスの方法を用いることが多いです。 `Parse`メソッドは汎用的で、`ParseExact`では特定の日付・時刻フォーマットを厳密に適用できます。時には`TryParse`メソッドを使うとエラー発生時にプログラムが停止せずに済むので有効です。

異なる国や地域では日付のフォーマットが異なるため、カルチャーを指定して解析することは国際化されたスクリプトには必須です。例えば、米国では`MM/dd/yyyy`ですが、日本では`yyyy/MM/dd`です。

精度が要求される場合や、特定の形式に対応する必要がある場合は`.NET`の`DateTimeOffset`や`TimeZoneInfo`を使用することで、タイムゾーンの情報を含めた解析も可能です。

## See Also (関連リンク)
- [DateTime.Parse メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.parse?view=net-6.0)
- [DateTime.ParseExact メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.parseexact?view=net-6.0)
- [カルチャに固有の日付と時刻の形式](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/standard-date-and-time-format-strings)
