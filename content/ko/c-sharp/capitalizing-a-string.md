---
title:                "C#: 문자열 대문자로 바꾸기"
simple_title:         "문자열 대문자로 바꾸기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜

문자열을 대문자로 변환하는 것에 대해 궁금하신가요? 이 블로그 포스트에서는 C# 코드를 사용하여 문자열을 대문자로 변환하는 방법을 알려드릴게요.

## 어떻게 하나요

문자열을 대문자로 변환하는 것은 매우 간단합니다. 먼저, `string` 타입의 변수를 만들고 원하는 값을 할당합니다. 그런 다음 `ToUpper()` 메소드를 사용하여 변수 내의 모든 문자를 대문자로 변환합니다.

```C#
string myString = "hello world";
string upperString = myString.ToUpper();
Console.WriteLine(upperString);
```

위의 코드를 실행하면 `HELLO WORLD`라는 출력 결과를 얻을 수 있습니다.

## 더 들어가보기

하지만 문자열을 대문자로 변환하는 것은 더 복잡합니다. 예를 들어, 한국어에서는 조사가 있기 때문에 모든 단어를 대문자로 변환한다면 올바로된 문장을 만들 수 없습니다. 이 경우 `CultureInfo` 클래스의 `TextInfo` 속성을 사용하면 각 언어에 맞게 문자열을 변환할 수 있습니다.

```C#
string myString = "안녕하세요";
CultureInfo cultureInfo = new CultureInfo("ko-KR");
TextInfo textInfo = cultureInfo.TextInfo;
string upperString = textInfo.ToUpper(myString);
Console.WriteLine(upperString);
```

위의 코드를 실행하면 `안녕하세요`가 아니라 `안녕하세요.`와 같은 출력 결과를 얻을 수 있습니다.

## 같이 보기

- [string.ToUpper 메소드 문서](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netcore-3.1)
- [CultureInfo 클래스 문서](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1)
- [TextInfo 속성 문서](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo?view=netcore-3.1)