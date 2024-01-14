---
title:                "C++: 부분 문자열 추출하기"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

제목: C++에서 Substring 추출하기
본문:

## 왜

C++에서 substring을 추출하는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 특정 문자열에서 원하는 부분만 뽑아서 사용하거나 검색할 때, 해당 문자열에서 특정 패턴을 찾기 위해 substring을 사용할 수 있습니다. 또한, substring을 추출하는 기능을 구현하면 코드의 유연성이 높아지고 보다 간편한 데이터 처리가 가능해집니다.

## 하는 법

Substring을 추출하는 방법은 간단합니다. 먼저, `string` 라이브러리를 include하고, 추출하고 싶은 문자열과 범위를 지정해주면 됩니다. 아래는 간단한 예제 코드입니다.

```C++
#include <string>
#include <iostream>

using namespace std;

int main(){
    // 문장 정의 및 출력
    string str = "안녕하세요, C++입니다.";
    cout << "원본 문장: " << str << endl;

    // substring 추출 후 출력
    string substring = str.substr(5, 2);
    cout << "추출한 substring: " << substring << endl;

    return 0;
}
```
출력 결과:
```
원본 문장: 안녕하세요, C++입니다.
추출한 substring: 세요
```

위 예제 코드에서 사용된 `str.substr(5, 2)` 부분에서 첫 번째 인자는 시작 위치를, 두 번째 인자는 추출하고 싶은 문자열의 길이를 나타냅니다. 따라서, 위 예제에서는 `5`번째 위치에서부터 `2`개의 문자를 추출하여 `substring` 변수에 할당한 것입니다.

위 코드를 실행하면 입력한 문자열의 일부가 추출된 것을 확인할 수 있습니다.

## 깊이 알아보기

`string` 라이브러리에는 substring을 추출하는 여러 가지 함수들이 존재합니다. 위에서 사용한 `substr()` 함수외에도 `substr(pos)`와 `substr(pos, len)` 함수가 있습니다. `substr(pos)` 함수는 해당 위치부터 문자열 끝까지 모든 문자를 추출하는 것이고, `substr(pos, len)` 함수는 해당 위치부터 길이만큼의 문자를 추출하는 것입니다.

또한, `find()` 함수를 사용하여 문자열에서 특정 패턴을 찾은 후 그 위치에 따라 `substr()` 함수를 사용해 원하는 부분만 추출할 수도 있습니다.

## 참고자료

- [C++ substr() function](https://www.programiz.com/cpp-programming/library-function/string/substr)
- [C++ find() and substr() functions](https://www.geeksforgeeks.org/cpp-find-and-rfind/)
- [문자열 추출하기 (C++)](https://boycoding.tistory.com/215)
- [C++ standard library](https://en.cppreference.com/w/cpp/header/string)

감사합니다!

## 참고자료

- [C++ substr() 함수](https://www.programiz.com/cpp-programming/library-function/string/substr)
- [C++ find()와 substr() 함수](https://www.geeksforgeeks.org/cpp-find-and-rfind/)
- [문자열 추출하기 (C++)](https://boycoding.tistory.com/215)
- [C++ 표준 라이브러리](https://en.cppreference.com/w/cpp/header/string)