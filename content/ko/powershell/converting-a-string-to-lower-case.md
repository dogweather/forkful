---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요?
문자열을 소문자로 변환한는 것은 모든 대문자를 소문자로 바꾸는 과정입니다. 대소문자를 구분하지 않는 검색이나 비교를 하거나, 일관된 출력을 보장하기 위해 프로그래머들은 이를 사용합니다.

## 어떻게 작성하나요:
PowerShell에서는 문자열을 소문자로 변환하기 위한 간편한 메소드인 'ToLower()'를 제공합니다. 

```PowerShell
$originalString = "Hello, World!"
$lowerCaseString = $originalString.ToLower()
$lowerCaseString
```

위 코드를 실행하면, 결과는 다음과 같이 출력됩니다.

```PowerShell
hello, world!
```

## Deep Dive
소문자 변환은 컴퓨터 프로그래밍에서 오래 동안 이용되어 왔고, 대부분의 프로그래밍 언어가 빠르고 효동적인 방법으로 이를 지원합니다. PowerShell은 문자열 메소드인 'ToLower()'를 통해 이 기능을 수행합니다. 다른 언어, 예를 들어 Python에서는 `.lower()`, Java에서는 `.toLowerCase()`를 사용해 동일한 기능을 시행합니다.

변환 작업은 문자열 내의 각 문자에 대해 Unicode 값을 찾아 소문자 Unicode 값으로 대체하는 방식으로 일어납니다. 때문에 변환 작업은 시간과 메모리를 어느 정도 사용하며, 큰 크기의 문자열에서는 이를 주의해야 합니다.

## 참고 문헌 
1. [PowerShell ToLower() 메소드](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
2. [Python lower() 메소드](https://docs.python.org/3/library/stdtypes.html#str.lower)
3. [Java toLowerCase() 메소드](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)