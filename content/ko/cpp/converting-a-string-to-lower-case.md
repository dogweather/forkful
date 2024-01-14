---
title:    "C++: 문자열 소문자로 변환하기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜

문자열을 소문자로 변환하는 것에 대해 왜 관심을 갖게 될까요? 대부분의 프로그래밍 언어에서 문자열은 매우 중요한 데이터 유형입니다. 그들 중 하나는 대소 문자를 구분한다는 것입니다. Python과 같은 언어에서는 이로 인해 인코딩 문제와 같은 문제가 발생할 수 있습니다. 따라서 대부분의 경우 문자열을 소문자로 변환하는 것은 유용한 프로그래밍 기술입니다.

# 방법

이제 실제로 문자열을 소문자로 변환하는 방법을 살펴보겠습니다. 다음은 기본 C++ 코드입니다.

```C++
#include <iostream>
#include <cstring>

using namespace std;

int main() {
    // 대문자로 이루어진 문자열 선언
    char str[] = "HELLO WORLD";
    // 문자열 길이 저장
    int len = strlen(str);
    // 문자열을 소문자로 변환
    for(int i=0; i<len; i++) {
        str[i] = tolower(str[i]);
    }
    // 변환된 문자열 출력
    cout << "Converted string: " << str;
    return 0;
}
```

위 코드를 실행하면 다음과 같은 출력이 나타납니다.

```
Converted string: hello world
```

위 코드에서는 C++의 `tolower()` 함수를 사용하여 대문자를 소문자로 변환하였습니다. 이를 사용하는 것은 간단하지만, 문자열의 모든 문자를 하나씩 반복해야하기 때문에 성능상으로 효율적이지 않을 수 있습니다. 이런 경우 Boost 라이브러리의 `to_lower()` 함수를 사용하여 더 쉽고 효율적으로 소문자로 변환할 수 있습니다.

## 딥 다이브

이제 문자열을 소문자로 변환하는 더 깊은 내용을 살펴보겠습니다. C++에서 모든 문자는 ASCII 코드로 표현됩니다. 따라서 `tolower()` 함수는 해당 문자를 적절한 ASCII 코드로 변환하여 소문자로 만들 수 있게 됩니다. 이 함수는 `locale` 객체를 사용하여 해당 문자의 로케일 정보를 얻고, 이를 기반으로 소문자를 정확하게 변환합니다.

또한, C++11부터는 `std::transform()` 함수를 사용하여 소문자 변환이 가능합니다. 이 함수는 모든 문자를 반복하지 않고도 문자열을 제공된 함수로 변환할 수 있습니다.

# 참고 자료

- [C++ locale](https://www.cplusplus.com/reference/locale/)
- [Boost to_lower() 함수](https://www.boost.org/doc/libs/1_75_0/doc/html/boost_locale/reference/to_lower.html)
- [std::transform() 함수](https://en.cppreference.com/w/cpp/algorithm/transform)

# 더 알아보기