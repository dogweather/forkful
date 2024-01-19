---
title:                "문자열 대문자로 바꾸기"
html_title:           "PowerShell: 문자열 대문자로 바꾸기"
simple_title:         "문자열 대문자로 바꾸기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 대문자화는 문자의 첫 글자를 대문자로 변경하는 것입니다. 이는 프로그래머들이 식별자 이름이나 사용자 입력을 표준화하는데 유용합니다.

## 어떻게:

```PowerShell
$string = "hello world"
$capitalizedString = (Get-Culture).TextInfo.ToTitleCase($string.ToLower())
Write-Output $capitalizedString
```
출력 결과는: `Hello World`

## Deep Dive:

문자열 대문자화는 프로그래밍 언어의 기본 기능 중 하나이며 대부분의 언어에는 이와 유사한 기능이 내장되어 있습니다. PowerShell에서는 .NET의 Get-Culture 함수와 TextInfo의 ToTitleCase 메서드를 사용하여 이를 구현합니다.

대안은 문자열을 소문자로 변환한 뒤 각 단어의 첫 문자를 대문자로 변경하는 것입니다. 그러나 이 방법은 모든 단어의 첫 글자를 대문자로 변경하게 됩니다.

## 참고 자료:

- [PowerShell 문자열 다루기](https://m.blog.naver.com/PostView.naver?isHttpsRedirect=true&blogId=kim_seok_bae&logNo=221388282285)
- [.Net의 ToTitleCase 메서드에 대한 MSDN 문서](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase)

## 총정리:

이 기사에서는 PowerShell에서 문자열 대문자화를 사용하는 방법과 이에 대한 깊이 있는 정보를 배웠습니다. 이 때 배운 지식을 활용해 본인의 PowerShell 스크립트 개발을 더 효과적으로 수행할 수 있길 바랍니다. Kotlin, JavaScript 등 다른 언어에서도 비슷한 방식으로 이 기능을 활용할 수 있습니다. 시작은 어렵지만 연습하면 능력을 향상시킬 수 있습니다. 행운을 빕니다!