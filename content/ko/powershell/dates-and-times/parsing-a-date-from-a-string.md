---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:09.040931-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8 \uD615\uC2DD\uC73C\uB85C \uC791\uC131\
  \uB41C \uB0A0\uC9DC\uB97C PowerShell\uC774 \uC774\uD574\uD558\uACE0 \uC791\uC5C5\
  \uD560 \uC218 \uC788\uB294 \uB0A0\uC9DC \uB370\uC774\uD130 \uD0C0\uC785\uC73C\uB85C\
  \ \uC778\uC2DD\uD558\uACE0 \uBCC0\uD658\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uB85C\uADF8 \uD30C\uC77C, \uC0AC\uC6A9\
  \uC790 \uC785\uB825 \uB610\uB294 \uB370\uC774\uD130 \uCC98\uB9AC\uB97C \uB2E4\uB8E8\
  \uB294 \uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uC77C\uBC18\uC801\uC778 \uC791\uC5C5\
  \uC778 \uB0A0\uC9DC\uB97C \uC870\uC791\uD558\uACE0,\u2026"
lastmod: '2024-03-13T22:44:55.560996-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD55C\uB2E4\
  \uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8 \uD615\uC2DD\uC73C\uB85C \uC791\uC131\uB41C\
  \ \uB0A0\uC9DC\uB97C PowerShell\uC774 \uC774\uD574\uD558\uACE0 \uC791\uC5C5\uD560\
  \ \uC218 \uC788\uB294 \uB0A0\uC9DC \uB370\uC774\uD130 \uD0C0\uC785\uC73C\uB85C \uC778\
  \uC2DD\uD558\uACE0 \uBCC0\uD658\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uB85C\uADF8 \uD30C\uC77C, \uC0AC\uC6A9\uC790\
  \ \uC785\uB825 \uB610\uB294 \uB370\uC774\uD130 \uCC98\uB9AC\uB97C \uB2E4\uB8E8\uB294\
  \ \uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uC77C\uBC18\uC801\uC778 \uC791\uC5C5\uC778\
  \ \uB0A0\uC9DC\uB97C \uC870\uC791\uD558\uACE0,\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 무엇을 하며 왜 하나?
문자열에서 날짜를 파싱한다는 것은 텍스트 형식으로 작성된 날짜를 PowerShell이 이해하고 작업할 수 있는 날짜 데이터 타입으로 인식하고 변환하는 것을 말합니다. 프로그래머는 로그 파일, 사용자 입력 또는 데이터 처리를 다루는 스크립트에서 일반적인 작업인 날짜를 조작하고, 포맷하고, 비교하거나 계산하기 위해 이 작업을 합니다.

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
