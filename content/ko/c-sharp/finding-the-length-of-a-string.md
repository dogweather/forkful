---
title:                "C#: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜라면: 문자열 길이를 찾는 일에 관심을 가지는 이유는 무엇인가요?

문자열 길이를 찾는 것은 프로그래밍에서 가장 기본적이고 필수적인 작업 중 하나입니다. 우리는 종종 사용자로부터 입력 받은 문자열의 길이를 알아야 하거나, 특정 문자열의 길이를 비교해야 할 때가 있습니다. 이를 위해 문자열의 길이를 찾는 방법을 공부하면, 보다 정확하고 빠른 코딩이 가능해집니다.

## 어떻게 하나요?

우선 C#에서 문자열의 길이를 찾는 방법을 알아보겠습니다. 아래의 코드는 문자열의 길이를 나타내는 빌트인 함수인 `Length`를 이용하는 예시입니다.

```C#
string myString = "안녕하세요";
int myStringLength = myString.Length;
Console.WriteLine("문자열의 길이는 {0}입니다.", myStringLength);
```

위의 코드를 실행하면 다음과 같은 결과가 출력됩니다.
```
문자열의 길이는 5입니다.
```

또한 다음과 같은 방법으로도 문자열의 길이를 찾을 수 있습니다.

```C#
string myString = "안녕하세요";
int myStringLength = 0;
foreach (char c in myString)
{
  myStringLength++;
}
Console.WriteLine("문자열의 길이는 {0}입니다.", myStringLength);
```

이 코드도 위와 같은 결과가 출력됩니다.

## 깊게 들어가기

C#에서 `Length` 함수는 문자열의 길이를 바이트 단위로 반환합니다. 그렇기 때문에 한글과 같은 다중 바이트 문자열이 포함되어 있다면, 실제 문자 수와 다를 수 있습니다. 따라서 `Length` 함수를 사용할 때는 이 점을 유의하는 것이 좋습니다.

## 관련 자료

- [C# 문자열 길이 찾기](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.length?view=netframework-4.8)
- [C# 문자열 관련 함수들](https://www.c-sharpcorner.com/uploadfile/mahesh/string-related-functions-in-C-Sharp/)