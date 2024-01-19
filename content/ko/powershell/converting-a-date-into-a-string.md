---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜를 문자열로 변환하는 것은 날짜 정보를 텍스트 형태로 바꾸는 것입니다. 프로그래머는 사용자가 읽기 쉬운 표현으로 날짜를 전달하거나, 파일 이름과 같은 곳에 날짜를 포함하기 위해 이렇게 합니다.

## 방법:

다음은 Powershell에서 날짜를 문자열로 변환하는 방법입니다.

```PowerShell
$date = Get-Date
$dateToString = $date.ToString("yyyy-MM-dd")
```

이 코드는 오늘의 날짜를 가져와 "yyyy-MM-dd" 형식으로 변환합니다.

결과는 아래와 같습니다:

```PowerShell
Write-Output $dateToString
2022-03-31
```

## 상세 정보:

날짜를 문자열로 변환하는 작업은 프로그래밍의 초기부터 수행해 왔습니다. 이는 사용자와 시스템이 날짜 정보를 쉽게 읽고 작성하도록 만드는 기본적인 방법입니다.

이 작업에는 여러 가지 대안이 있습니다. 예를 들어, ToString 메서드 외에도 -f 연산자를 사용하여 날짜를 문자열로 변환할 수 있습니다.

```PowerShell
$date = Get-Date
$dateToString = '{0:yyyy-MM-dd}' -f $date
```

또한 시간대를 추가하려면 ToString()에 "zzz"를 포함하면 됩니다.

```PowerShell
$dateToString = $date.ToString('yyyy-MM-dd zzz')
```

## 참고:

Powershell에 관한 추가 정보는 다음 웹사이트를 참조하십시오:

- 날짜 및 시간 문자열 형식 지정(Microsoft Docs): https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/standard-date-and-time-format-strings
- Get-Date cmdlet(Microsoft Docs): https://docs.microsoft.com/ko-kr/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1