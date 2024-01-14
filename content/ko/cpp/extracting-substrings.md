---
title:                "C++: 문자열 추출하기"
simple_title:         "문자열 추출하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

C++ 프로그래밍에 대해 잠깐 생각해보면, 원하는 데이터만 추출해야 할 때가 있습니다. 예를 들어, 사용자로부터 전화번호를 입력 받았을 때, "-" 기호를 제거하고 숫자만을 추출하고 싶을 수 있습니다. 이런 경우, substring 추출이 필요합니다. 

## 하는 법

Substring 추출을 위해서는 C++의 ```substr``` 함수를 사용하면 됩니다. 아래는 예시 코드입니다.

```C++
#include <iostream>
using namespace std;

int main() {
  string str = "안녕하세요!";
  
  // "녕하세요"만 받아오기
  string sub1 = str.substr(2);
  cout << sub1 << endl;
  
  // "안녕"만 받아오기
  string sub2 = str.substr(0, 2);
  cout << sub2 << endl;
  
  // "세요"만 받아오기
  string sub3 = str.substr(3, 2);
  cout << sub3 << endl;
  
  return 0;
}
```

위 코드의 결과는 다음과 같습니다.

```
녕하세요!
안녕
세요
```

위와 같이, ```substr``` 함수는 인덱스 번호를 이용하여 특정 부분의 문자열을 추출해줍니다. 자세한 인자 정보는 레퍼런스를 참고하세요.

## 깊이 파고들기

```substr``` 함수는 실제로는 C++의 ```string``` 클래스에 속해 있는 멤버 함수입니다. 따라서, 전체 동작 방식은 ```string``` 클래스의 멤버 함수인 ```substr```를 참고해야 합니다. 또한, 언어적인 차이, 예외처리 등에 대해서도 고려해야 합니다. 

## 그 외 참고

[C++ reference - substr](https://www.cplusplus.com/reference/string/string/substr/)