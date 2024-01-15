---
title:                "문자열의 길이 찾기"
html_title:           "C#: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것에 참여하는 이유는 매우 다양합니다. 예를 들어, 사용자가 입력한 정보의 길이를 제한하기 위해, 또는 특정 문자열 처리 작업을 수행하기 위해 등 다양한 상황에서 문자열의 길이를 찾아야 할 수 있습니다.

## 어떻게 작업 할까요?

문자열의 길이를 찾기 위해서는 C#에서 자주 사용되는 메소드 중 하나인 `string.Length`를 사용해야 합니다. 아래의 코드 예제를 참고하여 실제로 문자열의 길이를 찾는 작업을 해보세요.

```C#
// 문자열 변수 선언
string myString = "안녕하세요!";

// `Length` 메소드를 사용하여 문자열의 길이를 찾고 출력하기
Console.WriteLine($"문자열의 길이는 {myString.Length}자 입니다.");
```

위 코드의 결과는 다음과 같을 것입니다:

```
문자열의 길이는 6자 입니다.
```

## 깊이 파고들기

`string.Length` 메소드는 사실 C#에서 `string` 타입의 내장된 속성입니다. 따라서 별다른 설정 없이도 해당 메소드를 바로 사용할 수 있습니다. 이 속성은 문자열의 길이를 찾는 데에 매우 유용한 도구입니다. 그러나 조심해야 할 몇 가지 사항이 있습니다.

먼저, `string.Length`는 공백도 하나의 문자로 간주합니다. 따라서 공백이 있는 문자열의 경우, 실제 문자열의 길이보다 한 글자가 더 작게 나올 수 있습니다. 또한, `string.Length`는 영문자를 기준으로 하여 글자의 수를 세기 때문에, 한글과 같은 다른 언어의 문자열의 경우에는 원하는 결과를 얻지 못할 수 있습니다. 이에 따라서, 언어에 따라 적절한 메소드를 사용하여 문자열의 길이를 찾아야 합니다.

## 더 읽어보기

- [C# 문자열의 길이 찾기 - Microsoft Documentation](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.length?view=net-5.0)
- [C# 자습서 - 문자열 - Microsoft Documentation](https://docs.microsoft.com/ko-kr/dotnet/csharp/tutorials/working-with-strings)