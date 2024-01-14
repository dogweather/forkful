---
title:    "C++: 문자열 연결하기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
문자열을 연결하는 것에 대해 깊게 고민을 해본 적이 있습니까? 여러분이 어떤 이유로 문자열을 연결해야 하는지 궁금하시다면, 이 블로그 포스트를 읽어보세요!

## 방법
```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string name = "John";
    string greeting = "Hello, ";
    string message = greeting + name;
    cout << message << endl;
    return 0;
}
```

위 코드는 문자열을 연결하는 가장 간단한 방법을 보여줍니다. 우선 `<string>` 헤더 파일을 포함해야 합니다. 그 후에는 `string` 자료형을 사용하여 변수를 선언하고, `+` 연산자를 사용하여 문자열을 연결할 수 있습니다.

위 코드를 실행하면 `"Hello, John"`이라는 출력 결과를 얻을 수 있습니다.

## 깊게 들어가기
C++에서는 문자열을 다루는 여러 가지 방법이 있습니다. 다른 언어에서는 `+` 연산자 외에도 `concat()` 함수 등을 사용할 수 있지만, C++에서는 `+` 연산자만 사용할 수 있습니다.

또한, 문자열을 연결할 때에는 주의할 점이 있습니다. 두 문자열을 합친 결과가 오버플로우(overflow)가 발생할 수 있기 때문입니다. 따라서, 항상 충분한 크기의 버퍼가 할당되어 있는지 확인해주는 것이 중요합니다.

## 더 보기
[https://www.geeksforgeeks.org/concatenate-strings-in-c/](https://www.geeksforgeeks.org/concatenate-strings-in-c/)