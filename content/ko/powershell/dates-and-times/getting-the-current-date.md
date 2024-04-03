---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:41.630585-07:00
description: "\uBC29\uBC95: PowerShell\uC740 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744\
  \ \uAC00\uC838\uC624\uB294 \uBA85\uB839\uC5B4(cmdlets)\uB97C \uAC04\uB2E8\uD558\uAC8C\
  \ \uC81C\uACF5\uD569\uB2C8\uB2E4. `Get-Date` cmdlet\uC740 \uC774 \uBAA9\uC801\uC744\
  \ \uC704\uD55C \uC8FC\uC694 \uB3C4\uAD6C\uC785\uB2C8\uB2E4. \uC804\uCCB4 \uB0A0\uC9DC\
  \uC640 \uC2DC\uAC04\uC744 \uBC18\uD658\uD560 \uC218 \uC788\uC73C\uBA70, \uD544\uC694\
  \uC5D0 \uB530\uB77C \uD615\uC2DD\uC744 \uBCC0\uACBD\uD558\uAC70\uB098 \uC870\uC791\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.562681-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC740 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uAC00\uC838\uC624\
  \uB294 \uBA85\uB839\uC5B4(cmdlets)\uB97C \uAC04\uB2E8\uD558\uAC8C \uC81C\uACF5\uD569\
  \uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 방법:
PowerShell은 날짜와 시간을 가져오는 명령어(cmdlets)를 간단하게 제공합니다. `Get-Date` cmdlet은 이 목적을 위한 주요 도구입니다. 전체 날짜와 시간을 반환할 수 있으며, 필요에 따라 형식을 변경하거나 조작할 수 있습니다.

```powershell
# 현재 날짜와 시간을 가져옵니다.
Get-Date
```

**샘플 출력:**

```
2023년 9월 5일 화요일 오전 9:46:02
```

또한 출력 형식을 지정하여 필요한 정보만 표시하도록 할 수 있습니다. 예를 들어 날짜만 또는 시간만 표시할 수 있습니다.

```powershell
# 특정 형식으로만 현재 날짜를 가져옵니다.
Get-Date -Format "yyyy-MM-dd"
```

**샘플 출력:**

```
2023-09-05
```

```powershell
# 현재 시간만 가져옵니다.
Get-Date -Format "HH:mm:ss"
```

**샘플 출력:**

```
09:46:02
```

### .NET 클래스 사용하기
PowerShell은 .NET 클래스에 직접 액세스를 허용하여 날짜와 시간을 다루는 또 다른 방법을 제공합니다.

```powershell
# .NET DateTime 클래스를 사용하여 현재 날짜와 시간을 가져옵니다.
[System.DateTime]::Now
```

**샘플 출력:**

```
2023년 9월 5일 화요일 오전 9:46:02
```

UTC 시간의 경우:

```powershell
# .NET DateTime 클래스를 사용하여 현재 UTC 날짜와 시간을 가져옵니다.
[System.DateTime]::UtcNow
```

**샘플 출력:**

```
2023년 9월 5일 화요일 오후 1:46:02
```

이러한 명령어와 클래스는 날짜와 시간을 다루는 데 있어 강력하고 유연한 옵션을 제공하며, 많은 스크립팅 및 자동화 작업에 필수적입니다.
