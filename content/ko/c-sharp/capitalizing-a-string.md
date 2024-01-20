---
title:                "문자열 대문자화"
html_title:           "C#: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

문자열 대문자화는 모든 문자를 대문자로 바꾸는 것을 말합니다. 프로그래머들은 사용자 입력에서 일관성을 유지하거나, 텍스트를 비교할 때 대소문자를 무시하기 위해 이를 사용합니다.

## 어떻게 사용하는가:

다음은 C#에서 문자열을 대문자로 변경하는 방법을 보여주는 코드 예제입니다:

```C#
string originalText = "hello world";
string upperCaseText = originalText.ToUpper();

Console.WriteLine(upperCaseText);
```

이 코드를 실행하면 다음과 같은 출력이 나옵니다:

```
HELLO WORLD
```

## 깊이 있는 분석:

문자열 대문자화는 매우 오래전부터 사용되어 왔습니다. C#에서는 `ToUpper` 메서드를 사용하여 쉽게 구현할 수 있습니다. 물론, `TextInfo.ToTitleCase` 같은 대안도 있지만, 이는 첫 글자만 대문자로 변경합니다.

C#의 `ToUpper` 메서드는 원본 문자열을 수정하지 않습니다. 대신 새로운 문자열을 생성하여 반환합니다. 따라서 원본 문자열이 대문자화 과정에 영향을 받지 않습니다.

## 참고 자료:

다음 자료에서 더 많은 정보를 얻을 수 있습니다:
- [Microsoft - ToUpper 메서드](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.toupper)
- [Microsoft - ToTitleCase 메서드](https://docs.microsoft.com/ko-kr/dotnet/api/system.globalization.textinfo.totitlecase)