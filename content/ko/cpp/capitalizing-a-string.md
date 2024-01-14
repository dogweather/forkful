---
title:                "C++: 문자열 대문자로 변환하기"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

소문자 문자열의 첫 글자를 대문자로 바꾸는 일은 일상적인 프로그래밍 작업입니다. 예를 들어, 이름의 첫 번째 글자를 대문자로 바꾸어서 보기 좋게 출력하는 경우가 대표적인 예시입니다. 이를 위해 C++에서는 쉬운 방법을 제공하고 있습니다.

## 하는 방법

아래의 예시 코드를 참고하여 소문자 문자열의 첫 번째 글자를 대문자로 바꾸는 방법을 살펴보겠습니다. 

```C++
#include <iostream>
#include <cstring>
using namespace std;

// 문자열의 첫 번째 글자를 대문자로 바꾸는 함수
string capitalize(string str) {
    // 첫 번째 글자를 대문자로 변환
    str[0] = toupper(str[0]);
    
    return str;
}

int main() {
    // 소문자로 된 문자열 입력 받기
    string input;
    cout << "소문자로 된 문자열을 입력하세요: ";
    cin >> input;
    
    // 함수를 사용하여 첫 번째 글자 대문자로 변환 후 출력
    cout << capitalize(input) << endl;
    
    return 0;
}

```

### 출력:

```bash
소문자로 된 문자열을 입력하세요: blog
Blog
```

## 깊이 들어가보기

위의 예시 코드에서 사용된 `toupper()` 함수는 C++에서 제공하는 라이브러리 함수 중 하나입니다. 이 함수는 매개변수로 받은 문자를 대문자로 변환하여 반환합니다. `toupper()` 함수 외에도 `tolower()` 함수를 사용하면 소문자를 대문자로 변환하는 것 외에도 대문자를 소문자로 바꾸는 것도 가능합니다. 또한, 문자열의 첫 번째 글자 뿐만 아니라 원하는 위치의 글자를 대소문자로 변환하는 것도 가능합니다.

## 한 줄로 끝내기

C++에서 제공하는 라이브러리 함수를 사용하면 손쉽게 소문자 문자열의 첫 번째 글자를 대문자로 바꿀 수 있습니다.

## 관련 링크

- [C++ Reference: toupper()](https://www.cplusplus.com/reference/cctype/toupper/)
- [C++ Reference: tolower()](https://www.cplusplus.com/reference/cctype/tolower/)
- [GeeksforGeeks: How to capitalize first letter of a string in C++?](https://www.geeksforgeeks.org/capitalize-first-letter-of-a-string-in-c/)