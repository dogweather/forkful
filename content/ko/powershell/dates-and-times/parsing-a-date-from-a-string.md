---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:09.040931-07:00
description: "\uC5B4\uB5BB\uAC8C: PowerShell\uC740 \uD45C\uC900 \uB0A0\uC9DC \uD615\
  \uC2DD\uC5D0 \uC798 \uB9DE\uB294 `Get-Date` cmdlet\uACFC `[datetime]` \uD0C0\uC785\
  \ \uC561\uC140\uB7EC\uB808\uC774\uD130\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\
  \uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uC27D\uAC8C \uD30C\uC2F1\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4. \uB354 \uBCF5\uC7A1\uD558\uAC70\uB098 \uBE44\uD45C\uC900 \uB0A0\
  \uC9DC \uBB38\uC790\uC5F4\uC758 \uACBD\uC6B0, \uC815\uD655\uD55C \uD615\uC2DD\uC744\
  \ \uC9C0\uC815\uD558\uAE30 \uC704\uD574\u2026"
lastmod: '2024-03-13T22:44:55.560996-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC740 \uD45C\uC900 \uB0A0\uC9DC \uD615\uC2DD\uC5D0 \uC798 \uB9DE\
  \uB294 `Get-Date` cmdlet\uACFC `[datetime]` \uD0C0\uC785 \uC561\uC140\uB7EC\uB808\
  \uC774\uD130\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\
  \uC9DC\uB97C \uC27D\uAC8C \uD30C\uC2F1\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 어떻게:
PowerShell은 표준 날짜 형식에 잘 맞는 `Get-Date` cmdlet과 `[datetime]` 타입 액셀러레이터를 사용하여 문자열에서 날짜를 쉽게 파싱할 수 있습니다. 더 복잡하거나 비표준 날짜 문자열의 경우, 정확한 형식을 지정하기 위해 `[datetime]::ParseExact` 메소드를 사용할 수 있습니다.

### `Get-Date` 및 `[datetime]` 사용하기:
```powershell
# Get-Date을 사용한 간단한 변환
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**샘플 출력:**
```
Saturday, April 1, 2023 12:00:00 AM
```

```powershell
# 타입 액셀러레이터 [datetime] 사용하기
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**샘플 출력:**
```
Saturday, April 1, 2023 12:00:00 AM
```

### 비표준 형식을 위한 `[datetime]::ParseExact` 사용하기:
자동으로 인식되지 않는 형식의 경우, 정확한 파싱을 보장하기 위해 정확한 형식을 정의할 수 있습니다.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**샘플 출력:**
```
Saturday, April 1, 2023 2:00:00 PM
```

### 서드파티 라이브러리 활용하기
PowerShell 자체는 날짜 파싱에 매우 강력하지만, 매우 복잡한 시나리오나 추가 기능을 위해서는 NodaTime과 같은 .NET 라이브러리를 탐색할 수 있습니다. 그러나 많은 전형적인 사용 사례의 경우, PowerShell의 네이티브 기능으로 충분할 것입니다.

```powershell
# 설명을 위해 NodaTime 사용, 프로젝트에 라이브러리를 추가해야 한다는 점을 유의하세요
# Install-Package NodaTime -Version 3.0.5
# NodaTime을 사용하여 날짜 파싱하기
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**주의 사항:** 위 코드는 개념적인 설명입니다. 실제로는 NodaTime이 프로젝트에 올바르게 추가되어 타입과 방법이 사용 가능한지 확인하세요.
