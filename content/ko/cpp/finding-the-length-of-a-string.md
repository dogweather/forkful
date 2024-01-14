---
title:                "C++: 문자열의 길이 찾기"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 알아내는 것이 왜 중요한지 궁금한 적이 있으신가요? 문자열은 프로그래밍에서 매우 중요한 역할을 하며, 그 길이를 알아내는 것은 다양한 작업을 수행하는 데 매우 유용합니다. 따라서 우리는 이번 포스트에서 C++에서 문자열의 길이를 찾는 방법을 알아보겠습니다.

## 방법

우선, 문자열의 길이를 찾는 가장 간단한 방법은 `string` 자료형의 `length()` 함수를 사용하는 것입니다. 아래는 그 예시 코드입니다.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "안녕하세요";
    int length = str.length();
    cout << "문자열의 길이는 " << length << " 입니다." << endl;
    return 0;
}
```

위 코드를 실행하면 다음과 같은 출력이 나옵니다.

```
문자열의 길이는 5입니다.
```

또 다른 방법으로는 C 언어에서 사용되는 `strlen()` 함수를 사용하는 방법도 있습니다. 이 함수를 사용하기 위해 `<cstring>` 헤더 파일을 포함시켜야 합니다. 아래는 그 예시 코드입니다.

```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
    char str[] = "안녕하세요";
    int length = strlen(str);
    cout << "문자열의 길이는 " << length << " 입니다." << endl;
    return 0;
}
```

위 코드를 실행하면 결과는 동일하게 나옵니다. 하지만 이 방법은 C 스타일의 문자열만을 다루기 때문에, 보통 `string` 자료형의 `length()` 함수를 선호합니다.

## 깊이 들어가기

우리는 `length()` 함수를 이용해 문자열의 길이를 알아내는 것 외에도, `size()` 함수를 이용해 길이를 알아낼 수도 있습니다. `length()` 함수와 동일한 역할을 하지만, 다른 점은 `size()` 함수는 `string` 자료형의 `size_type` 데이터 타입을 반환한다는 것입니다.

또한, 위에서 알아본 방법 외에도 다양한 방법으로 문자열의 길이를 찾는 것이 가능합니다. 예를 들어, 반복문을 통해 하나씩 문자를 읽어나가면서 길이를 세는 방법이 있습니다. 하지만 이 방법은 보다 복잡하고 시간도 더 많이 소모하므로, `length()` 함수나 `size()` 함수를 사용하는 것이 바람직합니다.

## 관련 포스트

* [C++ 자습서](https://www.youtube.com/watch?v=vLnPwxZdW4Y) - 문자열의 길이 찾기 방법을 비롯한 C++의 기본 개념을 자세히 다루고 있는 유튜브 영상입니다.
* [C++ Reference](https://en.cppreference.com/w/) - C++에서 사용할 수 있는 다양한 함수들의 명세를 살펴볼 수 있습니다. 여기서도 `length()` 함수와 `size()` 함수에 대한 정보를 찾을 수 있습니다.