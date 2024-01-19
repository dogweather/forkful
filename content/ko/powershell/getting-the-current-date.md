---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

---
# PowerShell로 현재 날짜 얻기
---
## 무엇 & 왜?

현재 날짜를 얻는 것이란, 우리가 사용중인 컴퓨터의 시간에 따라 현재의 결정적인 날짜가 무엇인지 계산하는 것입니다. 이를 통해 우리는 로그를 구분하거나, 시간 간격을 계산하거나, 특정 일에 자동화 작업을 실행할 수 있습니다.

## 어떻게:

이런 식으로 명령어를 배치해 보세요.

```PowerShell
# 현재 날짜 및 시간을 얻습니다.
$현재일시 = Get-Date
echo $현재일시
```

샘플 출력은 다음과 같습니다:

```PowerShell
2021-09-15T14:32:00.3647008+09:00
```

## 깊게 들어가보기

PowerShell에서 현재 날짜를 얻는 방법은 오래 전부터 표준화되어 왔습니다. 이렇게 간단한 작업을 수행하기 때문에, 이제는 거의 모든 프로그래밍 언어에서 이와 유사한 함수나 메서드를 제공하고 있습니다. 

PowerShell은 다른 방식으로도 날짜를 얻을 수 있습니다. 예를 들어, .NET Framework의 DateTime 클래스를 직접 사용할 수도 있습니다:

```PowerShell
$현재일시 = [DateTime]::Now
echo $현재일시
```

또한 "Get-Date" cmdlet는 현재 시스템의 지역 설정에 따라 날짜 및 시간을 반환합니다. 

## 참고 자료

1. [PowerShell 공식 문서](https://docs.microsoft.com/en-us/powershell/)
2. [Get-Date 공식 문서](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7)

이런 간략하고 핵심적인 스타일을 선호하는 여러분에게 유용한 정보가 되었기를 바랍니다.