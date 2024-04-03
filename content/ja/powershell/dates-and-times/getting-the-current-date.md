---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:39.361199-07:00
description: "\u65B9\u6CD5: PowerShell\u306F\u3001\u65E5\u4ED8\u3068\u6642\u523B\u3092\
  \u53D6\u5F97\u3059\u308B\u305F\u3081\u306E\u76F4\u611F\u7684\u306A\u30B3\u30DE\u30F3\
  \u30C9\u30EC\u30C3\u30C8\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002`Get-Date`\
  \ \u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\u306F\u3001\u3053\u306E\u76EE\u7684\
  \u306E\u305F\u3081\u306E\u4E3B\u8981\u306A\u30C4\u30FC\u30EB\u3067\u3059\u3002\u5FC5\
  \u8981\u306B\u5FDC\u3058\u3066\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3084\u64CD\u4F5C\
  \u3092\u884C\u3046\u3053\u3068\u304C\u3067\u304D\u308B\u3001\u5B8C\u5168\u306A\u65E5\
  \u4ED8\u3068\u6642\u523B\u3092\u8FD4\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.451268-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u306F\u3001\u65E5\u4ED8\u3068\u6642\u523B\u3092\u53D6\u5F97\u3059\
  \u308B\u305F\u3081\u306E\u76F4\u611F\u7684\u306A\u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\
  \u30C8\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002`Get-Date` \u30B3\u30DE\
  \u30F3\u30C9\u30EC\u30C3\u30C8\u306F\u3001\u3053\u306E\u76EE\u7684\u306E\u305F\u3081\
  \u306E\u4E3B\u8981\u306A\u30C4\u30FC\u30EB\u3067\u3059\u3002\u5FC5\u8981\u306B\u5FDC\
  \u3058\u3066\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3084\u64CD\u4F5C\u3092\u884C\u3046\
  \u3053\u3068\u304C\u3067\u304D\u308B\u3001\u5B8C\u5168\u306A\u65E5\u4ED8\u3068\u6642\
  \u523B\u3092\u8FD4\u3057\u307E\u3059."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 方法:
PowerShellは、日付と時刻を取得するための直感的なコマンドレットを提供しています。`Get-Date` コマンドレットは、この目的のための主要なツールです。必要に応じてフォーマットや操作を行うことができる、完全な日付と時刻を返します。

```powershell
# 現在の日付と時刻を取得
Get-Date
```

**サンプル出力:**

```
2023年9月5日 火曜日 9:46:02 AM
```

出力をフォーマットして、必要な情報のみを表示することもできます。例えば、日付のみや時間のみです。

```powershell
# 特定のフォーマットで現在の日付のみを取得
Get-Date -Format "yyyy-MM-dd"
```

**サンプル出力:**

```
2023-09-05
```

```powershell
# 現在の時刻のみを取得
Get-Date -Format "HH:mm:ss"
```

**サンプル出力:**

```
09:46:02
```

### .NETクラスの使用
PowerShellは.NETクラスに直接アクセスを許可し、日付と時刻を扱う別の方法を提供します。

```powershell
# .NET DateTimeクラスを使用して現在の日付と時刻を取得
[System.DateTime]::Now
```

**サンプル出力:**

```
2023年9月5日 火曜日 9:46:02 AM
```

UTC時刻の場合:

```powershell
# .NET DateTimeクラスを使用して現在のUTC日付と時刻を取得
[System.DateTime]::UtcNow
```

**サンプル出力:**

```
2023年9月5日 火曜日 1:46:02 PM
```

これらのコマンドとクラスは、PowerShellでの日付と時刻の操作を強力かつ柔軟に行うためのものであり、多くのスクリプティングや自動化タスクに不可欠です。
