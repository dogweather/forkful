---
title:    "C++: 문자열의 길이 찾기"
keywords: ["C++"]
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것에 참여하는 이유는 다양합니다. 예를 들어, 프로그래밍에서 문자열을 다루는 많은 작업에는 해당 문자열의 길이를 알아야 할 때가 있습니다. 또한 애플리케이션의 사용자 입력을 검증하거나 적합한 형식으로 변환하기 위해 문자열 길이를 확인하는 데도 사용될 수 있습니다.

## 어떻게

```C++
#include <iostream>
using namespace std;

int main() {
  // 문자열 입력 받기
  string str;
  cout << "문자열을 입력하세요: ";
  getline(cin, str);

  // 문자열의 길이 출력하기
  cout << "입력한 문자열의 길이는 " << str.length() << "입니다." << endl;

  return 0;
}
```

**출력:**

```
문자열을 입력하세요: 안녕하세요!
입력한 문자열의 길이는 6입니다.
```

## 깊이 살펴보기

문자열의 길이를 찾는 방법은 문자열의 끝을 나타내는 널 문자(null character)가 나올 때까지 각 문자를 하나씩 세는 것입니다. 따라서 문자열의 길이는 널 문자를 제외한 문자의 개수와 동일합니다.

문자열의 길이를 찾는 함수는 여러 분야에서 많이 사용되기 때문에, C++의 string 라이브러리에 내장되어 있습니다. 이 함수는 문자열의 길이를 반환하며, 매우 효율적으로 작동합니다.

## 더 알아보기

* [How to find the length of a string in C++](https://www.geeksforgeeks.org/how-to-find-the-length-of-a-string-in-c/)
* [C++ String Length](https://www.programiz.com/cpp-programming/string-length)
* [C++ Standard Library - String](https://en.cppreference.com/w/cpp/string/basic_string)