---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
문자열 보간(String Interpolation)은 코드에서 상수 대신 변수에 문자열을 삽입하는 방법입니다. 프로그래머는 읽기 쉽고 관리하기 쉬운 코드를 작성하기 위해 이를 사용합니다.

## 사용법:
의미 있는 예를 통해 이를 살펴봅시다.
```PowerShell
$name = "Jin"
$greeting = "안녕하세요, $name"
```
이것은 $greeting의 출력을 "안녕하세요, Jin"으로 만듭니다.

더 많은 변수를 추가하는 경우, 괄호를 사용할 수 있습니다.
```PowerShell
$name = "Jin"
$age = 25
$message = "안녕하세요, $($name). 당신의 나이는 $($age)살 입니다."
```
이 스크립트는 "안녕하세요, Jin. 당신의 나이는 25살 입니다." 라고 출력합니다.

## Deep Dive
### 역사적 맥락
문자열 보간은 고대 프로그래밍 언어부터 존재했으며, Python, JavaScript 및 PowerShell과 같은 최신 언어에서도 흔하게 볼 수 있습니다.

### 대체 방법
문자열 보간 외에도, 문자열 연결 또는 문자열 포맷 메서드를 이용할 수도 있습니다.
```PowerShell
#문자열 연결
$name = "Jin"
$message = "안녕하세요, " + $name

# 문자열 포맷 메서드
$name = "Jin"
$message = "안녕하세요, {0}" -f $name
```

### 구현 세부 사항
PowerShell에서는 `$` 기호를 통해 변수의 값을 참조하고, `$(...)`를 이용해 표현식 값을 참조합니다.

## 참고 자료
[PowerShell 공식 문서: 문자열 보간](https://docs.microsoft.com/ko-kr/powershell/scripting/learn/deep-dives/everything-about-string-substitutions?view=powershell-7.1)
[StackOverflow: 문자열 보간 Vs 문자열 연결](https://stackoverflow.com/questions/3913502/string-interpolation-vs-string-concatenation)