---
title:    "C#: 정규식 사용하기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜
정규 표현식을 사용하는 이유는 텍스트를 효율적으로 구분하거나 추출할 수 있기 때문입니다.

## 방법
다음은 C#에서 정규 표현식을 사용하는 예제 코드와 결과입니다.

```C#
// 이메일 주소에서 사용자 이름과 도메인을 추출하는 예제
string email = "example123@gmail.com";
Match match = Regex.Match(email, @"(.*)@(.*)");
string username = match.Groups[1].Value;
string domain = match.Groups[2].Value;

// 결과 출력
Console.WriteLine("사용자 이름: " + username); // 출력: example123
Console.WriteLine("도메인: " + domain); // 출력: gmail.com
```

## 깊이 파헤치기
정규 표현식은 다양한 패턴을 매칭하고 추출하는 강력한 도구입니다. 특수 문자, 반복, 그룹화 등 다양한 기능을 활용하여 정확하게 원하는 문자열을 찾을 수 있습니다. 하지만 정규 표현식의 문법은 다소 복잡하기 때문에 높은 수준의 이해도가 필요합니다. 더 깊이 파헤쳐보고 싶다면 다음 링크들을 참고해보세요.

## 참고 자료
- [C# 정규 표현식 개념 소개 (Microsoft Docs)](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [정규 표현식 체험하기 (Regex101)](https://regex101.com/)
- [C#에서 정규 표현식 사용하기 (C# 공식 문서)](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-examples)