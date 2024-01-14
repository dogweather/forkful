---
title:    "C#: 부분 문자열 추출하기"
keywords: ["C#"]
---

{{< edit_this_page >}}

# 왜
서브스트링을 추출하는 작업에 참여하는 이유는 다양합니다. 예를 들어, 문자열에서 특정한 부분만을 추출하고 싶거나, 문자열을 다루는 자신만의 고유한 방식이 있는 경우 등이 있을 수 있습니다.

## 추출하는 방법
서브스트링을 추출하는 것은 C#에서 매우 간단한 작업입니다. 문자열에서 특정한 위치부터 원하는 길이만큼을 추출하는 방법은 다음과 같습니다.

```C#
string str = "안녕하세요! 반가워요.";
string subStr = str.Substring(3, 5);
Console.WriteLine(subStr);
```

출력 결과는 다음과 같을 것입니다.

```
녕하세요
```

서브스트링 추출에는 여러 가지 다른 방법도 있습니다. 예를 들어, 인덱스를 이용하여 문자열을 나누거나 특정한 조건을 만족하는 문자열만을 추출하는 등의 방법이 있습니다.

## 자세히 살펴보기
서브스트링을 추출하는 작업은 문자열을 다루는 데 매우 중요한 역할을 합니다. 이 작업을 제대로 이해하고 활용할 수 있다면 문자열을 다루는 많은 작업에서 유용하게 사용할 수 있습니다.

또한, 서브스트링 추출과 관련된 다양한 기능을 더 알아보고 싶다면 C#의 공식 문서를 참고하는 것이 좋습니다.

# 관련 자료
- [C# 문자열 관련 문서](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/strings/)
- [서브스트링 추출 예제 코드](https://www.w3schools.com/cs/cs_strings_substring.asp)
- [서브스트링의 활용 예제](https://www.geeksforgeeks.org/c-sharp-substring-method/)