---
title:                "文字列から日付をパースする"
aliases:
- ja/powershell/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:30.498522-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から日付をパースする"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
文字列から日付を解析するとは、テキスト形式で書かれた日付をPowerShellが理解し、扱える日付データタイプに認識して変換することです。プログラマーは、これを行って日付を操作、フォーマット、比較、または計算することがあります。これらはログファイル、ユーザー入力、またはデータ処理を扱うスクリプトで一般的なタスクです。

## 方法:
PowerShellは`Get-Date`コマンドレットと`[datetime]`タイプアクセラレータを使用して文字列から日付を解析することを簡単にします。これらは標準的な日付フォーマットに対してはうまく機能します。より複雑な、または非標準の日付文字列については、`[datetime]::ParseExact`メソッドを利用して正確なフォーマットを指定することができます。

### `Get-Date`と`[datetime]`の使用:
```powershell
# Get-Dateを使用したシンプルな変換
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**サンプル出力:**
```
2023年4月1日 土曜日 0:00:00
```

```powershell
# タイプアクセラレータ[datetime]の使用
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**サンプル出力:**
```
2023年4月1日 土曜日 0:00:00
```

### 非標準フォーマットのための`[datetime]::ParseExact`の使用:
自動的に認識されないフォーマットの場合、正確な解析を保証するために正確なフォーマットを定義できます。
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**サンプル出力:**
```
2023年4月1日 土曜日 14:00:00
```

### サードパーティーライブラリの活用
PowerShell自身は日付の解析に非常に強力ですが、非常に複雑なシナリオや追加の機能については、NodaTimeのような.NETライブラリを探索することもあります。ただし、多くの典型的なユースケースでは、PowerShellのネイティブ機能で十分です。

```powershell
# 例示のためにNodaTimeを使用していますが、ライブラリをプロジェクトに追加する必要があります
# Install-Package NodaTime -Version 3.0.5
# NodaTimeを使用して日付を解析する
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**サンプルノート:** 上記のコードは概念的な説明です。実際には、NodaTimeがプロジェクトに正しく追加されて、タイプとメソッドが利用可能になるようにしてください。
