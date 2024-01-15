---
title:                "부분 문자열 추출하기"
html_title:           "C++: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 부분 문자열을 추출하는 것은 굉장히 유용한 기술입니다. 이를 통해 문자열에서 필요한 데이터만 추출하여 다른 작업에 사용할 수 있습니다.

## 하는 방법

우선 문자열을 생성하고, 추출할 부분 문자열의 시작 인덱스와 길이를 지정합니다. 그리고 `substr()` 함수를 사용하여 부분 문자열을 추출할 수 있습니다. 예를 들어, 다음과 같은 코드를 작성할 수 있습니다.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // 문자열 생성
    string str = "안녕하세요! C++ 프로그래밍을 배워봅시다.";

    // 부분 문자열 추출
    string substr = str.substr(8, 9); // "C++ 프로그래밍"을 추출

    // 추출한 부분 문자열 출력
    cout << "추출한 부분 문자열: " << substr << endl;

    return 0;
}
```

위 코드의 출력은 다음과 같습니다.

```
추출한 부분 문자열: C++ 프로그래밍
```

또 다른 방법으로는 `find()` 함수를 사용하는 것입니다. 이 함수는 원하는 부분 문자열이 시작하는 인덱스를 반환해주기 때문에, `substr()` 함수와 함께 사용할 수 있습니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // 문자열 생성
    string str = "안녕하세요! C++ 프로그래밍을 배워봅시다.";

    // 부분 문자열 추출을 위한 시작 인덱스 찾기
    int start = str.find("C++");

    // 추출할 부분 문자열의 길이 설정
    int length = 7;

    // 추출한 부분 문자열 출력
    cout << "추출한 부분 문자열: " << str.substr(start, length) << endl;

    return 0;
}
```

위 코드의 출력은 다음과 같습니다.

```
추출한 부분 문자열: C++ 프
```

## 자세히 알아보기

위 코드에서 사용된 `substr()` 함수는 `string` 클래스의 멤버 함수로, `string` 객체에서 원하는 부분 문자열을 추출하는 기능을 수행합니다. 이 함수는 두 개의 인자를 받는데, 첫 번째 인자는 추출할 부분 문자열의 시작 인덱스이고 두 번째 인자는 추출할 부분 문자열의 길이입니다. 두 번째 인자를 지정하지 않으면 시작 인덱스부터 문자열의 끝까지의 모든 문자열을 추출합니다. 그리고 `find()` 함수는 `string` 클래스의 멤버 함수로, 인자로 전달된 부분 문자열을 찾아 해당 인덱스를 반환해줍니다. 이를 이용하여 `substr()` 함수에 인자로 전달할 수 있는 것입니다.

## 관련 링크

- [C++ `string` 클래스 문서](https://www.cplusplus.com/reference/string/string/)
- [C++ `substr()` 함수 문서](https://www.cplusplus.com/reference/string/string/substr/)
- [C++ `find()` 함수 문서](https://www.cplusplus.com/reference/string/string/find/)
- [C++ 문자열 처리 관련 포스팅 (번역)](https://modoocode.com/306)