---
title:                "랜덤 숫자 생성하기"
html_title:           "PowerShell: 랜덤 숫자 생성하기"
simple_title:         "랜덤 숫자 생성하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 하는가?

랜덤 숫자 생성이란 무엇인가요? 간단히 말해서, 랜덤 숫자 생성은 컴퓨터가 사용자가 지정한 범위 내에서 매번 무작위로 숫자를 선택하도록 하는 것입니다. 왜 이런 일을 하는 걸까요? 일단은 보안이나 통계 분석 등 여러 가지 이유로 원하는 숫자를 무작위로 생성하는 것이 프로그래머들에게 유용하기 때문입니다.

## 진행 방법:

```PowerShell
# 0에서 9 사이의 무작위 숫자 생성
Get-Random -Minimum 0 -Maximum 10

# 시작과 끝 날짜 사이의 무작위 날짜 생성
Get-Random -Minimum (Get-Date -Date "01/01/2020") -Maximum (Get-Date -Date "12/31/2020")

# 범위 내에서 여러 개의 무작위 숫자 생성
Get-Random -Count 5 -Minimum 0 -Maximum 100
```

## 깊이 파고들기:

(1) 랜덤 숫자 생성은 1940년대부터 컴퓨팅 기술이 발달하면서 주요한 문제 중 하나였습니다. 그리고 그 문제를 해결하기 위해 많은 수의 알고리즘이 개발되어 현재에 이르렀습니다. (2) 랜덤 숫자 생성에는 다양한 방법이 존재하며, 컴퓨터와 다른 디바이스에서도 사용할 수 있습니다. 그 중 가장 일반적인 방법은 시간과 컴퓨터 자체로부터 무작위성을 받아오는 것입니다. 하지만 보다 더 안전하고 무작위성이 보장되는 알고리즘도 존재합니다. (3) PowerShell에서는 내장된 `Get-Random` cmdlet을 사용하여 간편하게 랜덤 숫자를 생성할 수 있습니다. 이 cmdlet에는 여러 가지 옵션들이 있으며, 사용자가 원하는 형태와 범위의 랜덤 숫자를 생성할 수 있도록 다양한 기능들이 제공됩니다.

## 관련 링크:

- [PowerShell Get-Random 문서](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Get-Random?view=powershell-7)
- [랜덤 숫자 생성 알고리즘에 대한 논문](https://www.random.org/analysis/)