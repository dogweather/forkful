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

## 무엇이며 왜?
검색 및 교체는 텍스트를 찾고 다른 텍스트로 대체하는 것을 의미합니다. 이는 프로그래머가 코드에서 특정한 부분을 더 효율적이고 빠르게 수정할 수 있도록 도와줍니다.

## 하는 방법:
```C#
// 예시 1: 문자열에서 특정 문자 찾아 바꾸기
string text = "안녕하세요! 반가워요.";
text = text.Replace("안녕하세요", "안녕하세요?"); // "안녕하세요? 반가워요." 출력

// 예시 2: 정규식을 이용한 패턴 매칭 후 대체
string text = "내 이메일 주소는 abc@xyz.com입니다.";
string pattern = "[a-z0-9]+@[a-z]+\.[a-z]+";
string replacement = "xyz@gmail.com";
Regex regex = new Regex(pattern);
text = regex.Replace(text, replacement); // "내 이메일 주소는 xyz@gmail.com입니다." 출력
```

## 깊게 들어가보기:
검색 및 교체는 오래된 기술이며 다양한 언어에서 지원됩니다. 프로그래머는 정규식을 사용하여 더 복잡한 패턴 매칭을 할 수 있습니다. 또한 많은 개발환경에서 검색 및 교체 기능을 제공하며, 명령줄 도구나 GitHub과 같은 협업 도구를 이용하여 코드 전체에서 텍스트를 검색하고 바꿀 수 있습니다.

## 관련 자료:
- [C# 문자열 검색/교체](https://docs.microsoft.com/ko-kr/dotnet/csharp/how-to/search-strings)
- [정규식에 대하여](https://medium.com/programmers-studio/%EA%B0%95%EC%9D%98-%EC%BD%94%EB%94%A9%EC%9D%84-%EB%A7%8C%EB%82%AC%EB%8B%A4%EB%A6%AC%EA%B8%B0-%EC%A0%95%EA%B7%9C%EC%8B%9D-regex-ecbab2a5b5d4)
- [GitHub 코드 검색 및 교체](https://docs.github.com/en/free-pro-team@latest/github/searching-for-information-on-github/searching-code)