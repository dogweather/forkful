---
title:                "디버그 출력 출력"
html_title:           "C++: 디버그 출력 출력"
simple_title:         "디버그 출력 출력"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜
디버그 출력을 하려는 이유는 주로 코드에서 발생하는 오류를 찾기 위해서입니다. 디버그 출력을 통해 프로그램이 실행하는 동안 어떤 값을 가지고 있는지 확인할 수 있으며, 이를 토대로 문제가 발생하는 부분을 쉽게 파악할 수 있습니다.

## 방법
디버그 출력을 하기 위해서는 `cout` 또는 `printf` 함수를 사용합니다. 아래는 `cout`를 사용한 예제 코드와 출력 결과입니다.

```C++
#include <iostream>

using namespace std;

int main()
{
    int num1 = 10;
    double num2 = 3.14;
    string str = "Hello World!";

    cout << "num1의 값은 " << num1 << "입니다." << endl;
    cout << "num2의 값은 " << num2 << "입니다." << endl;
    cout << "str의 값은 " << str << "입니다." << endl;

    return 0;
}
```

```
num1의 값은 10입니다.
num2의 값은 3.14입니다.
str의 값은 Hello World!입니다.
```

위 예제 코드에서 `cout`는 디버그 출력을 담당하는 `std` 네임스페이스에 속해있는 객체이며, `<<` 연산자를 사용하여 값을 출력합니다. `endl`은 줄바꿈을 나타내는 명령어입니다.

## 딥 다이브
디버그 출력을 할 때 주의해야 할 점은 너무 많은 출력이나 불필요한 출력을 하지 않는 것입니다. 너무 많은 출력을 하게 되면 불필요한 정보가 출력되어서 보기 어렵고 출력이 너무 길어질 수 있습니다. 따라서 디버그 출력을 추가할 때는 필요한 정보만 충분히 출력하도록 주의해야 합니다.

## 참고 자료
- [C++ 디버그 출력하는 방법](https://boycoding.tistory.com/212)
- [C++ 출력 관련 함수](https://www.cplusplus.com/reference/cstdio/printf/)
- [C++ 입출력 스트림](https://www.cplusplus.com/reference/iostream/)