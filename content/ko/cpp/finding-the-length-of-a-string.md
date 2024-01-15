---
title:                "문자열의 길이 찾기"
html_title:           "C++: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜?

문자열의 길이를 찾는 것은 프로그래밍에서 자주 사용되는 기본적인 작업입니다. 이를 통해 문자열을 조작하거나 비교하는 데 도움이 되며, 코드의 유연성을 높일 수 있습니다.

## 어떻게?

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {

  // 문자열의 길이를 저장할 변수와 문자열을 입력받을 변수 선언
  int length;
  string str;

  // 문자열 입력받기
  cout << "문자열을 입력하세요: ";
  cin >> str;

  // 문자열의 길이를 구해 변수에 저장
  length = str.length();

  // 결과 출력
  cout << "문자열의 길이는 " << length << "입니다." << endl;

  return 0;
}

```

**출력:**

```
문자열을 입력하세요: Hello, world!
문자열의 길이는 13입니다.
```

## 딥 다이브

C++의 내장 함수인 `length()`를 사용하면 문자열의 길이를 쉽게 구할 수 있습니다. 이러한 함수는 언어마다 다르지만 대부분의 프로그래밍 언어에서 문자열의 길이를 구하는 기능을 제공합니다. 하지만 이 함수는 공백 문자를 포함하여 모든 문자를 하나의 길이로 취급한다는 점에 유의해야 합니다. 예를 들어, `"Hello, world!"`라는 문자열의 길이는 13이지만 공백을 제외한 문자의 개수는 12입니다. 이러한 특성을 이해하고 적절하게 활용하면 문자열을 더욱 다양하게 다룰 수 있습니다.

## 참고 자료

- [C++ string length 구하기](https://blockdmask.tistory.com/316)
- [string length 이해하기](https://www.geeksforgeeks.org/understanding-strlen-function-in-c-cpp/)
- [C++ string class documentation](https://www.cplusplus.com/reference/string/string/)