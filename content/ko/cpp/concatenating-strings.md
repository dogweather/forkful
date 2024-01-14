---
title:                "C++: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
문자열을 이어붙이는 것이 중요한 이유는 일반적으로 문자열을 다룰 때 필요하기 때문입니다. 예를 들어, 사람 이름과 성을 합치거나 전화번호를 하이픈과 함께 표시하는 등 여러 가지 상황에서 문자열을 이어붙이는 것은 매우 중요합니다.

## 어떻게
문자열을 이어붙이는 방법은 C++에서는 "+" 기호를 사용하는 것입니다. 이 기호를 이용하여 두 문자열을 연결할 수 있습니다. 예를 들어:

```C++
#include <iostream>

using namespace std;

int main() {
  string firstName = "영희";
  string lastName = "김";
  string fullName = firstName + lastName;
  cout << "전체 이름: " << fullName << endl;
  return 0;
}
```

이 코드는 "영희김"이라는 결과를 출력합니다.

## 깊이 들어가보기
C++에서는 문자열을 이어붙이는 데에도 많은 방법이 있습니다. "+" 연산자 이외에도 "append()" 함수를 사용하여 문자열을 연결할 수 있습니다. 또한 문자열을 이어붙일 때 공백 또는 특정 문자를 추가하는 방법도 있습니다. 더 많은 예시와 설명은 [이 링크](https://www.cplusplus.com/reference/string/string/operator+=/)를 참고해주세요.

## 관련 자료
- [C++에서 문자열 이어붙이기 더 알아보기](https://modoocode.com/217)
- [C++ 문자열 이어붙이기 방법 비교](https://algorhythms.tistory.com/185)
- [C++ 문자열 이어붙이기 관련 유용한 팁들](https://www.geeksforgeeks.org/cpp-strings-append-and-string-join/)