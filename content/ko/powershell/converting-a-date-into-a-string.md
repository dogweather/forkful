---
title:                "날짜를 문자열로 변환하기"
html_title:           "PowerShell: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

날짜를 문자열로 변환하는 것은 다양한 프로그래밍 언어에서 자주 사용되는 작업입니다. 이를 통해 우리는 날짜 데이터를 더욱 쉽고 효율적으로 다룰 수 있습니다.

## 어떻게:

아래의 예제를 참고해보세요. 그리고 날짜를 문자열로 변환하는 방법을 알아봅시다.

```PowerShell
$date = Get-Date
Write-Host "Current date: $date"

# 날짜를 문자열로 출력하기
Write-Host "String date: $($date.ToString("MM/dd/yyyy"))"
# 출력 결과: 02/28/2021

# 다른 날짜 포맷으로 출력하기
Write-Host "String date: $($date.ToString("dddd, MMMM dd, yyyy"))"
# 출력 결과: Sunday, February 28, 2021
```

## 깊게 파헤쳐보기:

우리가 현재 사용하는 날짜 포맷은 그레고리오력에 기반한 세계적인 표준입니다. 그러므로 대부분의 프로그래밍 언어에서 이를 지원합니다. PowerShell에서는 ```ToString()``` 메소드를 통해 날짜를 문자열로 변환할 수 있습니다. 원하는 날짜 포맷을 지정하여 출력할 수도 있으며, 다양한 날짜 포맷 옵션을 찾아보는 것도 유용합니다. 하지만 주의할 점은 단일따옴표('')가 아니라 두 개의 쌍따옴표(")를 사용하는 것이 문자열로 변환할 때 유의미한 결과를 얻을 수 있습니다.

## 같이 보기:

- [PowerShell 날짜 포맷 가이드](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [PowerShell 문자열 형식 지정 지시어](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1#customdateandtimeformats)