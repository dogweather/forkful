---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "PowerShell: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
날짜를 미래나 과거로 계산하는 것은 특정 시점에서 이동한 시간을 측정하는 프로세스입니다. 프로그래머들은 이를 사용하여 경과된 시간을 추적하거나 후속 이벤트를 예측합니다.

## 이렇게 하세요:

PowerShell에서 미래나 과거의 날짜를 계산하는 방법을 알아보겠습니다.

```PowerShell
# 현재 날짜 가져오기
$now = Get-Date
# 일주일 후의 날짜 계산
$nextWeek = $now.AddDays(7)

# 출력
Write-Output "지금은 $now 입니다."
Write-Output "일주일 후는 $nextWeek 입니다."
```

이 스크립트를 실행하면 현재 날짜와 7일 후의 날짜가 출력됩니다.

## 깊이 분석:

데이터의 과거 또는 미래를 계산하는 것은 컴퓨터 프로그래밍의 역사와 동시에 시작된 기능 중 하나입니다. 이로 인해 프로그래머들은 예측과 추적을 편리하게 수행할 수 있는 도구를 손에 넣게 되었습니다.

이와 같은 프로세스에는 여러 가지 방법이 있을 수 있습니다. 타임스탬프를 사용하여 직접 계산을 수행하거나, `DateTime` 객체의 기능을 활용해 날짜와 시간을 조작하는 방법입니다.

PowerShell에서는 `DateTime` 객체를 사용하여 날짜와 시간 조작을 간편하게 수행할 수 있습니다. "AddDays" 메서드를 이용하면 원하는 만큼의 날짜를 더하거나 빼는 것이 가능합니다.

## 참고 자료:

다음 링크에서 PowerShell 및 날짜 및 시간 조작에 대한 더 자세한 정보를 얻을 수 있습니다.

- [PowerShell 공식 문서](https://docs.microsoft.com/ko-kr/powershell/)
- [System.DateTime 공식 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0)
- [날짜 및 시간을 조작하는 PowerShell 스크립트](https://devblogs.microsoft.com/scripting/manipulating-dates-and-times-in-powershell/)