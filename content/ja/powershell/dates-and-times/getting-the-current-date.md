---
title:                "現在の日付の取得"
date:                  2024-02-03T19:10:39.361199-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

PowerShellで現在の日付を取得することは、システムの現在の日付と時刻を取得することについてです。この操作は、ログ取り、タイミング操作、または日付に基づいて意思決定を行うなどのタスクにとって基本的なものです。プログラマーは、イベントを追跡し、タスクをスケジュールし、スクリプトやアプリケーションで日付固有のロジックを扱うためにこの機能を使用します。

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