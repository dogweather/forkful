---
title:                "C++: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 것에 대해 더 배우고 싶은 분들이 있을 수 있습니다. 이번 글에서는 C++ 프로그래밍에서 문자열을 소문자로 변환하는 이유와 방법에 대해 알아보겠습니다.

## 방법
문자열을 소문자로 변환하는 방법은 간단합니다. 다음과 같은 ```C++``` 코드를 사용할 수 있습니다.

```C++
#include <iostream>
#include <string>
#include <algorithm> // transform 함수를 사용하기 위해 사용

using namespace std;

int main() {
  string str = "HELLO WORLD!";

  // 모든 문자를 소문자로 변환
  transform(str.begin(), str.end(), str.begin(), ::tolower);

  // 결과 출력
  cout << str << endl;

  return 0;
}

```

위 코드를 실행하면 "hello world!"라는 출력을 얻을 수 있습니다. ```transform()``` 함수는 두 개의 문자열 반복자 범위와, 변환할 문자에 대한 함수를 매개변수로 받아 문자열을 변환합니다. 여기서는 모든 문자를 소문자로 변환하기 위해 ```::tolower``` 함수를 사용했습니다.

## 깊이 파헤치기
C++에서 문자열을 소문자로 변환하는 방법은 굉장히 다양합니다. 위 예제에서는 특정 함수를 사용해서 모든 문자를 소문자로 변환했습니다만, 다른 방법으로는 문자들을 하나씩 확인하면서 대문자를 소문자로 바꾸는 방법도 있습니다. 또한, 일부 문자는 ASCII 코드에 따라 기본적으로 소문자로 변환될 수 있습니다. 따라서 문자열을 소문자로 변환하는 다양한 방법을 알고 있어야 효율적인 코드를 작성할 수 있습니다.

## See Also
- [C++ string 클래스 문서](https://www.cplusplus.com/reference/string/string/)
- [C++ transform 함수 문서](https://www.cplusplus.com/reference/algorithm/transform/)