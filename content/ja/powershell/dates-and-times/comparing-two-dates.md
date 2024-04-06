---
date: 2024-01-20 17:33:45.905030-07:00
description: "How to: (\u65B9\u6CD5) PowerShell\u3067\u306F\u3001`Get-Date` \u30B3\
  \u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\u3092\u4F7F\u3063\u3066\u65E5\u4ED8\u3092\u53D6\
  \u5F97\u3057\u3001\u6BD4\u8F03\u6F14\u7B97\u5B50\u3092\u4F7F\u7528\u3057\u3066\u65E5\
  \u4ED8\u3092\u6BD4\u8F03\u3057\u307E\u3059\u3002\u3053\u306E\u6A5F\u80FD\u306F\u3001\
  \u521D\u671F\u306E\u30B3\u30DE\u30F3\u30C9\u30B7\u30A7\u30EB\u3084\u30D0\u30C3\u30C1\
  \u30D5\u30A1\u30A4\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u3067\u306F\u8907\u96D1\u3067\
  \u3057\u305F\u304C\u3001PowerShell\u306E\u5C0E\u5165\u3067\u7C21\u5358\u306B\u306A\
  \u308A\u307E\u3057\u305F\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.960814-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) PowerShell\u3067\u306F\u3001`Get-Date` \u30B3\u30DE\u30F3\
  \u30C9\u30EC\u30C3\u30C8\u3092\u4F7F\u3063\u3066\u65E5\u4ED8\u3092\u53D6\u5F97\u3057\
  \u3001\u6BD4\u8F03\u6F14\u7B97\u5B50\u3092\u4F7F\u7528\u3057\u3066\u65E5\u4ED8\u3092\
  \u6BD4\u8F03\u3057\u307E\u3059\u3002\u3053\u306E\u6A5F\u80FD\u306F\u3001\u521D\u671F\
  \u306E\u30B3\u30DE\u30F3\u30C9\u30B7\u30A7\u30EB\u3084\u30D0\u30C3\u30C1\u30D5\u30A1\
  \u30A4\u30EB\u30B9\u30AF\u30EA\u30D7\u30C8\u3067\u306F\u8907\u96D1\u3067\u3057\u305F\
  \u304C\u3001PowerShell\u306E\u5C0E\u5165\u3067\u7C21\u5358\u306B\u306A\u308A\u307E\
  \u3057\u305F\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
weight: 27
---

## How to: (方法)
```PowerShell
# 日付を作成
$date1 = Get-Date '2023-04-01'
$date2 = Get-Date '2023-05-01'

# 日付を比較
if ($date1 -lt $date2) {
    "date1 is earlier than date2"
} elseif ($date1 -gt $date2) {
    "date1 is later than date2"
} else {
    "date1 is the same as date2"
}
```

出力:
```
date1 is earlier than date2
```

## Deep Dive (詳細な情報)
PowerShellでは、`Get-Date` コマンドレットを使って日付を取得し、比較演算子を使用して日付を比較します。この機能は、初期のコマンドシェルやバッチファイルスクリプトでは複雑でしたが、PowerShellの導入で簡単になりました。

代替手法として、`.CompareTo()` メソッドや `[datetime]` クラスの静的メソッド `::Compare()` も存在します。しかし、比較演算子は読みやすく簡潔です。

実装については、PowerShell は背後で .NET の `DateTime` 型を使用しており、`.NET` の日付と時刻に関連するリッチな機能セットを活用しています。

## See Also (関連情報)
- [Get-Date コマンドレットの公式ドキュメンテーション](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date)
- [.NET の DateTime 構造についての公式ドキュメンテーション](https://docs.microsoft.com/dotnet/api/system.datetime)
