---
title:    "C++: 문자열 대문자로 바꾸기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

왜 누군가가 문자열을 대문자로 변환하는 작업에 참여하는지 이유는 다양합니다. 예를 들어, 사용자로부터 입력받은 문자열을 대문자로 변환해서 출력하는 프로그램을 만들고 싶은 경우가 있을 수 있습니다. 또는 대소문자를 구분하지 않는 검색 기능을 구현하거나, 문자열을 정렬하기 전에 대문자를 먼저 처리하는 등의 이유로 문자열을 대문자로 변환하는 작업이 필요할 수 있습니다.

## 사용 방법

씨++에서 문자열을 대문자로 변환하는 방법은 다소 복잡해 보일 수 있지만, 실제로는 간단합니다. 우선 문자열을 저장할 변수를 선언한 후, 해당 변수의 각 문자에 접근하여 아스키 코드를 활용해 대문자로 변환하는 방법을 사용합니다. 예를 들어, 아래 코드를 참고해 보세요.

```C++
#include <iostream>
using namespace std;

int main() {
    string str = "Hello, world!"; // 변환할 문자열
    for (int i = 0; i < str.length(); i++) { // 문자열의 길이만큼 반복
        if (str[i] >= 97 && str[i] <= 122) { // 소문자일 경우
            str[i] -= 32; // 대문자로 변환
        }
    }
    cout << str; // 변환된 문자열 출력
    return 0;
}
```

위 코드의 출력 결과는 다음과 같습니다.

```
HELLO, WORLD!
```

## 깊게 파고들기

문자열을 대문자로 변환하기 위해 사용된 아스키 코드는 대문자와 소문자 간의 차이가 32라는 것을 기억하고 있어야 합니다. 아스키 코드는 영문 알파벳의 대문자와 소문자, 그리고 특수 문자 등 각각에 대한 고유한 숫자를 할당해 놓은 것입니다. 따라서 대문자 소문자 간의 차이인 32를 더하거나 빼면 대소문자를 변환할 수 있습니다.

또한, 대문자와 소문자는 아스키 코드 뿐만 아니라 유니코드를 사용해도 변환이 가능합니다. 유니코드는 전 세계의 모든 문자를 컴퓨터에서 표현하기 위한 표준 코드 체계입니다.

## 참고자료

- [C++ Strings](https://www.programiz.com/cpp-programming/strings)
- [ASCII Table](https://www.asciitable.com/)
- [Unicode official website](https://www.unicode.org/)