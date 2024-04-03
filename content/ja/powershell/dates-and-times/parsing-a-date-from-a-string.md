---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:30.498522-07:00
description: "\u65B9\u6CD5: PowerShell\u306F`Get-\u2026"
lastmod: '2024-03-13T22:44:42.450446-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u306F`Get-Date`\u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\u3068\
  `[datetime]`\u30BF\u30A4\u30D7\u30A2\u30AF\u30BB\u30E9\u30EC\u30FC\u30BF\u3092\u4F7F\
  \u7528\u3057\u3066\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\
  \u308B\u3053\u3068\u3092\u7C21\u5358\u306B\u3057\u307E\u3059\u3002\u3053\u308C\u3089\
  \u306F\u6A19\u6E96\u7684\u306A\u65E5\u4ED8\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u306B\
  \u5BFE\u3057\u3066\u306F\u3046\u307E\u304F\u6A5F\u80FD\u3057\u307E\u3059\u3002\u3088\
  \u308A\u8907\u96D1\u306A\u3001\u307E\u305F\u306F\u975E\u6A19\u6E96\u306E\u65E5\u4ED8\
  \u6587\u5B57\u5217\u306B\u3064\u3044\u3066\u306F\u3001`[datetime]::ParseExact`\u30E1\
  \u30BD\u30C3\u30C9\u3092\u5229\u7528\u3057\u3066\u6B63\u78BA\u306A\u30D5\u30A9\u30FC\
  \u30DE\u30C3\u30C8\u3092\u6307\u5B9A\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\
  \u3059."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

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
