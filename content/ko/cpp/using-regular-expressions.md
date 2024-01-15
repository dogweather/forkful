---
title:                "Computer 도구를 사용하는 입장에서: 정규 표현식 활용"
html_title:           "C++: Computer 도구를 사용하는 입장에서: 정규 표현식 활용"
simple_title:         "Computer 도구를 사용하는 입장에서: 정규 표현식 활용"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

한번씩은 코딩을 할 때, 문자열을 다루는 경우가 있습니다. 예를 들어서 사용자로부터 입력받은 정보를 분석하거나 문자열에 대해 특정 조건을 검사하는 경우 등이죠. 이때 정규표현식을 사용하면 문자열을 더욱 쉽게 다룰 수 있습니다.

## 왜 정규표현식을 사용하는가?

정규표현식은 특정 패턴을 가진 문자열을 매칭해줍니다. 즉, 특정 문자열을 찾기 위해 사용할 수 있는 강력한 도구입니다. 이를 이용하면 복잡한 문자열을 쉽게 다룰 수 있습니다.

## 정규표현식을 사용하는 방법

아래 예시 코드들을 참고해서 정규표현식을 어떻게 사용하는지 알아보세요. 코드 블록은 ```C++ ... ``` 형식으로 나타내었습니다.

```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
    string str = "My email is abc@gmail.com";
    regex pattern("(\\w+)@(\\w+).(\\w+)");
    smatch match;

    while (regex_search(str, match, pattern)) {
        for (auto m : match)
            cout << m << " ";
        cout << endl;
        str = match.suffix().str();
    }
    return 0;
}
```

위 코드는 문자열에서 이메일 주소를 추출하는 예제입니다. ```regex``` 라이브러리를 사용해서 패턴을 정의하고, 이를 문자열에 적용합니다. 이메일 주소를 포함한 모든 문자열을 찾아서 출력하는 것을 확인할 수 있습니다.

```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
    string str = "I love C++, but sometimes it can be challenging.";
    regex pattern("C[+]");
    smatch match;

    while (regex_search(str, match, pattern)) {
        for (auto m : match)
            cout << m << " ";
        cout << endl;
        str = match.suffix().str();
    }
    return 0;
}
```

이번에는 문자열에서 특정 패턴인 "C++"를 찾는 예제입니다. 위 코드를 실행하면 "C++"를 포함한 모든 부분 문자열이 출력됩니다.

## 정규표현식의 깊은 이해

정규표현식을 더욱 잘 다루기 위해서는 패턴을 이해하는 것이 중요합니다. 우선 "w"는 알파벳, 숫자, 밑줄을 나타냅니다. 따라서 "(\w+)"는 이메일 주소에서 ```abc```와 ```gmail```을 매칭하지만 ```.com```은 매칭하지 않습니다. 괄호로 감싼 부분은 각각 group이라고 불리는데, 이를 이용하면 매칭된 결과를 더욱 다루기 쉽습니다.

컴파일러에서 정규표현식을 적용할 때, regex 패턴을 먼저 컴파일하고 나서 입력 문자열과 매칭하는 형식으로 작성하는 것이 권장됩니다. 또한 반복문을 사용해서 모든 매칭된 결과를 출력하는 것을 추천합니다.

## 추가 학습 자료

[regular expressions reference](https://www.regular-expressions.info/reference.html)  
[regex c++ reference](https://en.cppreference.com/w/cpp/regex)