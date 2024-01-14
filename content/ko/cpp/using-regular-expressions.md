---
title:                "C++: 정규 표현식 사용하기"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜 Regular Expressions을 사용할까요?

Regular Expressions은 컴퓨터 프로그래밍에서 중요한 역할을 담당하는 도구입니다. 이 도구를 사용하면 문자열에서 특정 패턴을 쉽게 찾아낼 수 있으며, 이를 통해 텍스트 처리 작업을 간단하고 효율적으로 수행할 수 있습니다. 또한 Regular Expressions을 사용하면 코드의 가독성과 유지 보수성을 향상시킬 수 있습니다.

## 사용 방법

Regular Expressions를 사용하기 위해서는 C++에서 제공하는 <regex> 라이브러리를 사용해야 합니다. 먼저 문자열에 포함된 패턴을 정의한 다음, 이 정의된 패턴을 사용하여 해당 패턴과 일치하는 문자열을 찾아내면 됩니다. 다음은 Regular Expressions을 사용해서 이메일 주소를 찾는 간단한 예제 코드입니다.

```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
    // 정규표현식 패턴 정의
    regex pattern("[a-z]+@[a-z]+\\.[a-z]+");
    // 검색할 문자열
    string text = "My email address is abc123@gmail.com";
    // 정규표현식과 일치하는 문자열 찾기
    smatch matches;
    regex_search(text, matches, pattern);
    // 결과 출력
    cout << "Found email address: " << matches.str() << endl; 
    return 0;
}
```

위 코드의 실행 결과는 다음과 같습니다.

```
Found email address: abc123@gmail.com
```

## 더 깊게 알아보기

Regular Expressions을 더 깊게 사용하기 위해서는 정규표현식의 문법과 기능을 잘 이해해야 합니다. 정규표현식을 작성할 때는 특수 문자, 문자 클래스, 양자법 등을 사용하여 패턴을 지정할 수 있습니다. 또한 정규표현식의 검색 결과를 다양한 방법으로 처리할 수 있습니다. 이를 통해 더욱 정교한 패턴을 찾아낼 수 있습니다.

## 더 많은 정보 보기

더 많은 정보를 알아보려면 다음 링크들을 참고해보세요.

[Regular Expressions - GeeksforGeeks](https://www.geeksforgeeks.org/regular-expressions-in-c/) <br/>
[regex - cppreference](https://en.cppreference.com/w/cpp/regex) <br/>
[정규표현식의 기본과 활용 - 나무위키](https://namu.wiki/w/%EC%A0%95%EA%B7%9C%ED%91%9C%ED%98%84%EC%8B%9D) <br/>

## 관련 링크

[Regular Expressions - 외부 링크](https://en.wikipedia.org/wiki/Regular_expression) <br/>
[정규표현식 사용 예제 - 외부 링크](https://www.tutorialspoint.com/regex/regex_examples.htm) <br/>
[정규표현식 문법 설명 - 외부 링크](https://www.rexegg.com/regex-quickstart.html)