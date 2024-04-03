---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:30.498522-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.450446-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u3067\u66F8\u304B\u308C\u305F\
  \u65E5\u4ED8\u3092PowerShell\u304C\u7406\u89E3\u3057\u3001\u6271\u3048\u308B\u65E5\
  \u4ED8\u30C7\u30FC\u30BF\u30BF\u30A4\u30D7\u306B\u8A8D\u8B58\u3057\u3066\u5909\u63DB\
  \u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u3053\u308C\u3092\u884C\u3063\u3066\u65E5\u4ED8\u3092\u64CD\u4F5C\u3001\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u3001\u6BD4\u8F03\u3001\u307E\u305F\u306F\u8A08\u7B97\
  \u3059\u308B\u3053\u3068\u304C\u3042\u308A\u307E\u3059\u3002\u3053\u308C\u3089\u306F\
  \u30ED\u30B0\u30D5\u30A1\u30A4\u30EB\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u3001\
  \u307E\u305F\u306F\u30C7\u30FC\u30BF\u51E6\u7406\u3092\u6271\u3046\u30B9\u30AF\u30EA\
  \u30D7\u30C8\u3067\u4E00\u822C\u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\u3002."
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
