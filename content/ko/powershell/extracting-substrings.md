---
title:                "부분 문자열 추출"
html_title:           "PowerShell: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

서브스트링 추출이란 무엇인가요? 이것은 문자열에서 일부분만을 추출하는 것을 의미합니다. 보통, 이 기능은 텍스트에서 원하는 정보만을 추출하기 위해 프로그래머들이 사용합니다.

## 해야 할 작업:

PowerShell에서 서브스트링 추출하는 방법은 다음과 같습니다.

```PowerShell
# 변수에 문자열 할당하기
$string = "안녕하세요, 저는 PowerShell을 공부하고 있습니다."

# 서브스트링 추출하기
$string.Substring(0, 5)  # 결과: 안녕하세요
$string.Substring(7, 2)  # 결과: 저는

# 변수 선언 없이 바로 추출하기
"PowerShell은 입문자들에게 유용합니다.".Substring(0, 10)  # 결과: PowerShell은
```

## 자세히 알아보기:

하지만 왜 서브스트링을 추출해야 할까요? 그 이유는 프로그래밍에서 가끔은 텍스트에서 특정 정보만을 필요로 할 때가 있기 때문입니다. 예를 들어, 사용자의 이름만 따로 추출해야 할 때가 있습니다. PowerShell 이외의 언어에서도 서브스트링 추출을 할 수 있지만, PowerShell을 사용하면 보다 간편하고 유용하게 추출할 수 있습니다.

서브스트링 추출은 보통 시작 지점과 끝 지점을 정해주는 것이 필요합니다. 그래서 시작 지점과 끝 지점이 따로 지정된 서브스트링 추출 함수가 있고, 시작 지점부터 특정 문자열의 위치를 찾아서 추출하는 함수도 있습니다.

## 관련 정보:

- [PowerShell 공식 문서](https://docs.microsoft.com/en-us/powershell/)
- [PowerShell 강의 영상](https://www.youtube.com/playlist?list=PL6lLwIYKxjOftnT4_XPyT3gdzk1q_hH4u)
- [PowerShell 웹사이트](https://www.powershell.com/)