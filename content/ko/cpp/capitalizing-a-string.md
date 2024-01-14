---
title:    "C++: 문자열 대문자로 바꾸기"
keywords: ["C++"]
---

{{< edit_this_page >}}

## 왜
문자열의 첫 글자를 대문자로 바꾸는 것에 대한 이유는 간단합니다. 코드의 가독성을 높이고, 프로그램의 사용성을 향상시키기 위해서입니다.

## 어떻게
C++에서 문자열을 대문자로 바꾸는 방법에 대해 알아보겠습니다. 먼저, ```toupper()``` 함수를 사용할 수 있습니다. 이 함수는 문자를 대문자로 바꾸는 역할을 합니다. 그리고 문자열의 첫 번째 글자를 대문자로 바꾸기 위해서는 첫 번째 문자를 ```toupper()``` 함수로 변환하고, 나머지 문자열을 그대로 유지하면 됩니다.

예를 들어, "hello"라는 문자열을 대문자로 바꾸고 싶다면 다음과 같이 코드를 작성할 수 있습니다.

```C++
string str = "hello";
str[0] = toupper(str[0]);
cout << str;
```

출력 결과는 "Hello"가 될 것입니다. 또 다른 방법으로는 C++의 표준 라이브러리 중 하나인 ```<algorithm>```을 사용하는 방법이 있습니다. 이 라이브러리에는 문자열을 대문자로 바꿔주는 ```toupper()``` 함수가 포함되어 있습니다. 따라서 아래와 같이 코드를 작성할 수 있습니다.

```C++
#include <iostream>
#include <algorithm>
using namespace std;

int main() {
    string str = "hello";
    transform(str.begin(), str.end(), str.begin(), ::toupper);
    cout << str;
    return 0;
}
```

출력 결과는 똑같이 "Hello"가 될 것입니다.

## 깊이 파고들기
물론 이렇게 대문자로 바꾸는 기능은 매우 간단한 작업처럼 보이지만, 내부적으로는 정말 깊이 있는 과정이 일어나고 있습니다. 단순히 첫 번째 문자를 대문자로 바꾸는 것이 아니라, 모든 문자를 검사하고 필요한 경우 대문자로 바꾸는 작업이 일어납니다. 이는 메모리의 재배치, 재할당, 그리고 문자열의 길이가 긴 경우 많은 연산이 필요할 수도 있습니다.

따라서 문자열을 대문자로 바꾸는 것은 간단한 작업처럼 보이지만 내부적으로는 많은 연산이 필요합니다. 이를 위해서는 C++의 표준 라이브러리를 제대로 이해하고, 적절한 함수를 사용하는 것이 중요합니다.

## 참고
- C++ toupper(): http://www.cplusplus.com/reference/cctype/toupper/
- C++ <algorithm> Library: http://www.cplusplus.com/reference/algorithm/