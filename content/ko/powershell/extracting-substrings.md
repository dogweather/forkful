---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요?
문자열에서 일부 문자를 추출하는 것을 'substring 추출'이라 합니다. 프로그래머들은 원하는 정보만을 찾아내거나 특정 패턴을 분석하기 위해 이를 사용합니다.

## 어떻게 사용하나요:
다음은 PowerShell에서 substring을 추출하는 방법입니다:

```PowerShell
$string = "PowerShell Scripting"
$substring = $string.substring(0, 9)
write-host $substring
```

실행 결과:

```PowerShell
PowerShell
```

여기서 substring 함수의 첫 번째 인자는 시작 위치를 나타내며, 두 번째 인자는 추출하려는 문자열의 길이를 나타냅니다.

## 딥다이브:
Substring 추출은 문자열의 일부분을 접근하고 처리하는 기본적인 프로그래밍 개념입니다. 문자열 처리는 빅데이터 분석, 자연어 처리 등의 분야에서 중요하게 사용됩니다. PowerShell에서는 .NET Framework의 String.Substring 메서드를 통해 이를 지원합니다. 

대안으로는 Split, Replace와 같은 다른 문자열 함수를 사용하거나 정규표현식을 이용하여 더 복잡한 패턴의 문자열을 추출할 수도 있습니다. 특히, 정규표현식은 유효성 검사, 검색 및 치환 등에서 널리 사용됩니다.

## 참고 자료:
1. [PowerShell Substring 메서드 문서](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0)
3. [자연어 처리를 위한 문자열 처리](https://realpython.com/natural-language-processing-spacy-python/)