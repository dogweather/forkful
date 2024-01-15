---
title:                "텍스트 검색 및 교체"
html_title:           "C#: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

검색 및 텍스트 교체를 진행하는 이유는 프로그램에서 특정한 문자 또는 단어를 자동으로 찾아 바꿀 수 있기 때문입니다.

## 어떻게

`C#`에서 `Regex` 클래스를 사용하면 간단하게 문자열에서 검색 및 교체를 할 수 있습니다. 아래의 예제 코드를 참고해보세요.

```C#
string originalText = "안녕하세요, 저는 프로그래머입니다.";
string replacedText = Regex.Replace(originalText, "프로그래머", "개발자");

Console.WriteLine(replacedText);
```

출력 결과는 `"안녕하세요, 저는 개발자입니다."`가 됩니다. 위 코드에서는 `Regex` 클래스의 `Replace` 메서드를 사용해 원본 문자열에서 특정 단어를 찾아 다른 단어로 바꾸는 작업을 수행하였습니다. 이 외에도 `Regex` 클래스에는 다양한 기능들이 있으니 자세한 사용법은 공식 문서를 참고해보세요.

## 딥 다이브

검색 및 교체 작업을 할 때 유용한 기능 중 하나는 정규식(Regular Expression)을 활용하는 것입니다. 정규식은 특정한 규칙을 가진 문자열의 패턴을 지정하여 검색하거나 교체할 수 있도록 도와줍니다. 예를 들어, `^` 기호를 이용하면 문자열의 시작부분만을 찾아내고, `$` 기호를 이용하면 문자열의 끝부분만을 찾아낼 수 있습니다. 또한 `+` 기호를 이용하면 해당 문자가 한 번 이상 나오는 부분을 찾을 수 있습니다. 이처럼 정규식을 잘 활용하면 더 다양한 문자열을 처리할 수 있으므로 익숙해지는 것이 좋습니다.

## 참고

- [C# Regex 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1)
- [정규식 표현 참고 자료(정규 표현식 30분만에 제대로 배우기)](https://wikidocs.net/259)