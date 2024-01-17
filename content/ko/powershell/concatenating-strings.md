---
title:                "문자열 연결하기"
html_title:           "PowerShell: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 연결은 간단하게 말하면 여러 개의 문자열을 하나로 합치는 것입니다. 프로그래머들은 이를 하는 이유는 가독성을 향상시키고, 데이터를 분석하기 쉽게 만들기 위해서입니다.

## 방법:

문자열 연결은 매우 간단한 작업입니다. 아래의 예시 코드를 참고하여 실제로 어떻게 수행하는지 살펴보세요:

```PowerShell
# 문자열 "Hello"와 "World"를 연결하는 예시 코드
$firstString = "Hello"
$secondString = "World"

# 두 문자열을 연결하여 "Hello World" 출력
Write-Host ($firstString + " " + $secondString)
```

위의 코드를 실행하면 "Hello World"라는 문자열이 출력됩니다.

## 깊이 파헤치기:

문자열 연결은 프로그래밍에서 매우 중요한 역할을 합니다. 예를 들어, 여러 개의 변수를 합쳐서 하나의 문자열로 만들거나, 사용자에게 보여줄 메시지를 만드는 등 여러 가지 상황에서 사용됩니다.

또한, PowerShell에서는 문자열 연결 대신 서식 문자열을 사용할 수도 있습니다. 서식 문자열은 다른 변수들을 문자열 안에 직접 넣을 수 있어서 보다 간편하게 사용할 수 있습니다.

마지막으로, PowerShell에서의 문자열 연결은 더하기 연산자(`+`)를 이용하여 수행할 수 있습니다. 이는 다른 프로그래밍 언어에서도 일반적으로 사용되는 방식입니다.

## 관련 자료:

- [문자열 연결하는 방법](https://docs.microsoft.com/ko-kr/powershell/module/microsoft.powershell.utility/join-path?view=powershell-7.1)
- [서식 문자열 사용하기](https://docs.microsoft.com/ko-kr/powershell/module/microsoft.powershell.utility/format-string?view=powershell-7.1)