---
title:                "현재 날짜 가져오기"
html_title:           "PowerShell: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
현재 날짜를 알아내는 것은 프로그래머들이 간단하게 처리할 수 있는 작업 중 하나입니다. 이 작업은 다양한 프로그래밍 작업에서 필수적이며, 데이터 분석이나 로그 관리와 같은 작업에서 활용될 수 있습니다.

## 어떻게:
```PowerShell
Get-Date
```

위의 코드를 실행하면 현재 날짜와 시간이 출력됩니다.

```PowerShell
Get-Date -Format "dddd, MMMM dd, yyyy"
```

위의 코드를 실행하면 현재 날짜가 "월요일, 1월 01, 2021"과 같은 형식으로 출력됩니다.

## 깊숙한 곳:
현재 날짜를 가져오는 것은 현대의 컴퓨터 시스템에서 가장 당연한 일 중 하나입니다. 하지만 예전에는 현재 날짜를 얻기 위해 다양한 방법이 사용되었습니다. 하드웨어의 시간을 읽어오는 방식이나 사용자가 직접 입력하는 방식 등이 주로 사용되었습니다.

현재 날짜를 가져오는 방법은 다양하지만 가장 일반적인 방법은 시스템의 시간과 날짜를 읽어오는 것입니다. 하지만 PowerShell에서는 다양한 포맷을 지원하여 원하는 형식으로 날짜를 출력할 수 있습니다.

## 참고 자료:
- [Microsoft 문서: Get-Date](https://docs.microsoft.com/ko-kr/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Rookie Note: Get-Date 사용 예제](https://rookienote.tistory.com/105)
- [Linux에서 현재 시간을 가져오는 방법](https://linuxize.com/post/how-to-get-current-date-time-in-bash/)