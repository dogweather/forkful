---
title:                "현재 날짜 가져오기"
aliases:
- ko/powershell/getting-the-current-date.md
date:                  2024-02-03T19:10:41.630585-07:00
model:                 gpt-4-0125-preview
simple_title:         "현재 날짜 가져오기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

PowerShell에서 현재 날짜를 검색하는 것은 시스템의 현재 날짜와 시간을 가져오는 것과 관련이 있습니다. 이 작업은 로깅, 타이밍 작업 또는 날짜를 기반으로 한 결정을 내리는 것과 같은 작업에 기본적입니다. 프로그래머들은 이 기능을 사용하여 이벤트를 추적하고, 작업을 스케줄하며, 스크립트와 응용 프로그램에서 날짜 관련 로직을 처리합니다.

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
