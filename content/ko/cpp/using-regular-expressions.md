---
title:                "C++: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

Korean Translation:
## 왜
정규 표현식을 사용하는 것이 왜 중요한지에 대해 1-2 문장으로 설명합니다.

## 사용 방법
"```C++ ...```" 코드 블록 내에서 코딩 예제와 샘플 출력을 포함합니다.
코드 예제:
```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
    string test_string = "Hello, world!";
    regex expression("[A-Za-z]+");

    sregex_iterator it(test_string.begin(), test_string.end(), expression);
    sregex_iterator end;

    while (it != end) {
        cout << it->str() << endl;
        ++it;
    }

    return 0;
}

// 출력:
// Hello
// world
```

## 깊이 파헤치기
정규 표현식을 사용하는 더 깊은 정보에 대해 설명합니다. 정규 표현식을 사용하면 문자열 내에서 특정 패턴을 찾거나 대체할 수 있습니다. 이를 통해 프로그램에서 문자열을 더욱 효율적으로 처리할 수 있으며, 데이터 유효성 검사, 스크립팅, 텍스트 처리 등 다양한 용도로 활용할 수 있습니다.

## 유사한 주제 더 알아보기
- [정규 표현식 기본 개념](https://ko.wikipedia.org/wiki/%EC%A0%95%EA%B7%9C_%ED%91%9C%ED%98%84%EC%8B%9D)
- [C++에서의 정규 표현식 사용 방법](https://www.geeksforgeeks.org/cpp-regex-tutorial/)
- [정규 표현식 패턴만들기](https://regexr.com/)
- [정규 표현식을 사용하여 문자열 다루기](https://www.hackerrank.com/domains/regex)
- [정규 표현식 고급 사용법](https://www.regular-expressions.info/tutorial.html)

## 더 알아보기
- [C++ Markdown 사용하기](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#code)
- [C++ 정규 표현식 사용 예제](https://www.tutorialspoint.com/cplusplus/cpp_regular_expressions.htm)
- [C++ 정규 표현식 문서](https://en.cppreference.com/w/cpp/regex)
- [C++로 쓰인 정규 표현식 비교](https://blog.stevenlevithan.com/archives/faster-javascript-regexp)