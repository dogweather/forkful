---
title:                "정규 표현식 사용하기"
html_title:           "Bash: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하는가?

정규 표현식은 텍스트 패턴을 일치시키는 코드 작성 방법입니다. 이는 복잡한 검색 및 변환 작업을 간결하게 만들기 때문에 프로그래머들이 많이 사용합니다.

## 이렇게 사용하자:

C++에서 정규 표현식을 사용하려면 `<regex>` 라이브러리를 참조해야 합니다. 아래에 기본적인 예시를 보여드리겠습니다:

```C++
#include<iostream>
#include<regex>

int main() {
  std::string s = "I love programming";
  std::regex e ("(programming)");

  // 패턴을 찾아 변환합니다.
  std::cout << std::regex_replace(s,e,"coding") << std::endl;
}
```
이 코드는 "I love programming"이란 문장에서 "programming"을 찾아 "coding"으로 바꿉니다. 그래서 출력 결과는 "I love coding"이 됩니다.

## 깊은 탐색

정규 표현식은 1950년대에 개발된 알고리즘으로, 크게 사용 방식이 변하지 않고 있습니다. C++에도 이전 버전에서 사용했던 것과 크게 다르지 않게 사용됩니다. 

정규 표현식의 대안으로는 문자열 검색 함수를 직접 만드는 것이 있지만, 이렇게 하면 코드가 복잡해지고 유지보수가 어려워집니다. 

C++의 정규 표현식 구현은 Perl과 매우 비슷합니다. 그래서 Perl에 익숙한 사람들이라면 C++에서도 쉽게 사용할 수 있습니다.

## 참고 자료

C++의 정규 표현식을 더 깊게 이해하려면 아래의 자료를 참고하세요:

1. [C++의 정규 표현식 라이브러리 문서](http://www.cplusplus.com/reference/regex/)
2. [정규 표현식에 관한 Wikipedia](https://ko.wikipedia.org/wiki/%EC%A0%95%EA%B7%9C_%ED%91%9C%ED%98%84%EC%8B%9D)
3. [정규 표현식을 이해하고 사용하는 방법](https://regexr.com/)