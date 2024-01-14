---
title:    "C++: 문자열의 길이 찾기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 알아내는 일에 참여하는 이유는 매우 간단합니다. 우리는 프로그래밍을 할 때 여러 가지 작업을 해야하며, 이 중에 문자열의 길이를 알아내야 하는 경우가 많기 때문입니다. 따라서 이 기능은 매우 유용하고 중요합니다.

## 어떻게

우리는 C++에서 문자열의 길이를 알아내는 데 사용할 수 있는 여러 가지 메소드를 가지고 있습니다. 여기서는 가장 기본적인 방법인 `length()` 메소드를 사용하여 문자열의 길이를 알아보겠습니다.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // 문자열 변수를 선언합니다.
    string str = "안녕하세요!";

    // `length()` 메소드를 사용하여 문자열의 길이를 알아냅니다.
    int length = str.length();

    // 결과를 출력합니다.
    cout << "문자열의 길이는 " << length << "입니다." << endl;

    return 0;
}

// 출력 결과:
// 문자열의 길이는 6입니다.
```

위의 예제에서는 `string` 라이브러리에서 제공하는 `length()` 메소드를 사용하여 문자열의 길이를 구했습니다. 하지만 다른 방법으로는 `size()` 메소드를 사용하는 것도 가능합니다.

```C++
int length = str.size();
```

두 가지 메소드 모두 동일한 결과를 반환하며, 어떤 것을 사용하더라도 상관없습니다.

## 딥 다이브

`length()` 메소드의 내부 동작을 조금 더 살펴보면, 문자열의 길이를 반환할 때 문자열의 마지막에 종료 문자인 `'\0'`을 포함하지 않는다는 것을 알 수 있습니다. 이는 C++에서 문자열을 처리하는 NULL 문자 때문입니다. 따라서 `length()` 메소드는 실제로 문자열 내의 문자 수를 반환하는 것이 아니라, 종료 문자 이전의 문자 수를 반환하는 것입니다.

많은 프로그래밍 언어에서는 이러한 종료 문자를 사용하여 문자열의 끝을 나타내지만, C++에서는 이를 명시적으로 포함해 주어야 합니다.

## 참고

- [C++ Tutorial: 문자열 (strings)](https://www.cplusplus.com/doc/tutorial/ntcs/)
- [C++ Reference: string::length](https://en.cppreference.com/w/cpp/string/basic_string/length)
- [C++ Reference: string::size](https://en.cppreference.com/w/cpp/string/basic_string/size)