---
title:                "문자열 소문자로 변환하기"
html_title:           "C#: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열을 소문자로 변환하는 것은 말 그대로 문자열을 모두 소문자로 바꾸는 것입니다. 프로그래머들은 이 작업을 자주 하게 됩니다. 이유는 간단합니다. 소문자로 된 문자열은 더 쉽게 처리할 수 있기 때문입니다.

## 방법:
```C#
string str = "HELLO WORLD";
string lowerCaseStr = str.ToLower();

Console.WriteLine(lowerCaseStr);

// Output: hello world
```

위의 예시 코드를 보면, 우선 소문자로 변환할 문자열을 변수에 저장합니다. 그리고 그 변수를 `ToLower()` 함수를 사용하여 소문자로 변환한 후, 결과를 콘솔에 출력하는 식으로 작성하였습니다.

## 깊이 파고들기:
소문자로 변환하는 기능은 C# 3.0 버전에서 처음 도입되었습니다. 이전에는 `.ToLower()` 함수 대신 `ToLowerInvariant()` 함수를 사용하여 문자열을 소문자로 변환하였습니다. 이 함수는 엔진의 언어 설정에 상관없이 항상 같은 결과를 출력하므로, 이를 사용하는 것이 더 안정적입니다.

다른 언어나 라이브러리에서도 문자열의 대소문자를 변환하는 방법이 있지만, C#의 `ToLower()` 함수는 가장 간단하고 편리한 방법입니다. 이를 사용하면 프로그래머들이 간단하게 문자열의 대소문자를 다룰 수 있게 되었으며, 코드도 더 깔끔해졌습니다.

## 더 알아보기:
- [.NET 문자열 관련 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.string)
- [C# 3.0 버전 소개](https://docs.microsoft.com/ko-kr/dotnet/csharp/whats-new/csharp-version-history#c-version-30)
- [C# 전체 지원 문자 집합](https://msdn.microsoft.com/ko-kr/library/cww5e6k9(v=vs.90).aspx)