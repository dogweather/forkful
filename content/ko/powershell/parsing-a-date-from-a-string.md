---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?

문자열에서 날짜를 파싱하는 것은 일련의 문자로 표현된 날짜를 구체적인 날짜 데이터로 변환하는 과정입니다. 이를 수행하는 이유는 문자열로 표현된 날짜 데이터를 다루기 쉽고 간편하게 사용하도록 하기 위함입니다.

## 어떻게 하는가?

다음은 PowerShell에서 문자열에서 날짜를 파싱하는 두가지 방법을 제시한 코드 예시입니다.

```PowerShell
# Method 1
$dateString = "2020-10-25"
[DateTime]$date = $dateString
Write-Output $date

# Output: Sunday, October 25, 2020 12:00:00 AM

# Method 2
$dateString = "2020/10/25"
$date = [DateTime]::Parse($dateString)
Write-Output $date

# Output: Sunday, October 25, 2020 12:00:00 AM
```
이 스크립트를 수행하면 "2020-10-25"와 "2020/10/25" 두 문자열이 모두 2020년 10월 25일과 같은 날짜로 변환됩니다.

## 깊이 분석

날짜를 문자열로부터 파싱하는 작업은 많은 프로그래밍 언어에서 꽤 많이 진행해 왔습니다. 이는 주로 문자열로 저장되어 있는 날짜 데이터를 컴퓨터가 이해하고 처리할 수 있는 형태로 변환하는 데 필요한 작업입니다. 

대안으로는 날짜와 시간을 직접 숫자로 표현하는 방법이 있습니다만, 이는 다양한 날짜 형식을 처리하는 데 다소 어려움이 있을 수 있습니다. 따라서 다양한 형식의 날짜 문자열을 제대로 파싱하고 처리하기 위해 date parsing이 필요합니다.

PowerShell에서 날짜 파싱은 .NET의 DateTime 클래스를 이용하여 진행합니다. 파싱 함수인 [DateTime]::Parse()는 여러 형식의 날짜 문자열을 다룰 수 있어 매우 유용합니다. 

## 참고자료 
