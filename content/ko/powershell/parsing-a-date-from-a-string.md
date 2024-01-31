---
title:                "문자열에서 날짜 파싱하기"
date:                  2024-01-20T15:37:44.486328-07:00
html_title:           "Arduino: 문자열에서 날짜 파싱하기"
simple_title:         "문자열에서 날짜 파싱하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가 & 왜?)
문자열에서 날짜를 분석하는 것은 문자열 데이터를 날짜 타입으로 변환하는 과정입니다. 프로그래머는 데이터 형식을 통일하고, 날짜와 시간 연산을 가능하게 하기 위해 이 작업을 합니다.

## How to: (어떻게 하나요?)
```PowerShell
# 간단한 날짜 문자열을 DateTime 객체로 변환
$dateString = "2023-04-01"
$parsedDate = [datetime]::ParseExact($dateString, "yyyy-MM-dd", $null)

# 결과 출력
$parsedDate
```

```
Saturday, April 1, 2023 12:00:00 AM
```

```PowerShell
# 형식이 지정된 날짜와 시간 문자열을 DateTime 객체로 변환
$dateTimeString = "2023년 04월 01일 14시 30분 00초"
$parsedDateTime = [datetime]::ParseExact($dateTimeString, "yyyy년 MM월 dd일 HH시 mm분 ss초", $null)

# 결과 출력
$parsedDateTime
```

```
Saturday, April 1, 2023 2:30:00 PM
```

## Deep Dive (심층 분석)
PowerShell에서 날짜를 파싱하는 역사는 `.NET Framework`의 DateTime 구조체와 밀접하게 연관되어 있습니다. 예전에는 `Get-Date`와 같은 cmdlet을 이용해서 날짜를 파싱했지만, 현재는 `[datetime]::ParseExact`와 같은 `.NET` 메서드를 직접 사용하여 더 정확하고 유연한 날짜 파싱이 가능합니다.

다른 방법론으로는 `DateTime.Parse`, `DateTime.TryParse`, `Convert.ToDateTime` 등 다양한 메서드를 사용할 수 있으나 `ParseExact`를 사용하면 좀 더 정교한 형식 지정과 오류를 방지할 수 있어서 많이 선호됩니다. 실제 구현은 내부적으로 문자열을 분석하고 날짜와 시간의 각 구성 성분을 추출하여 DateTime 객체를 생성합니다.

## See Also (더 보기)
- [Microsoft Docs on ParseExact](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact)
- [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
- [Understanding the DateTime Structure](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
