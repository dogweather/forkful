---
title:    "C++: Abbreviation: 표준 오류로 작성하기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜
표준 오류에 쓰기를 하는 이유는 무엇일까요? 간단한 1-2 문장으로 설명하겠습니다.

컴퓨터 프로그래밍에서 디버깅은 매우 중요한 단계입니다. 만약 에러가 발생했을 때, 우리는 그 에러를 찾아서 고쳐야 합니다. 디버깅을 할 때, 프로그래머들은 대부분 표준 출력을 사용하게 됩니다. 하지만 표준 오류에 쓰기를 하는 것은 더욱 간편하고 효과적인 방법입니다. 함수나 클래스의 실행 과정을 디버깅하거나 프로그램의 잘못된 부분을 찾는데 표준 오류를 사용하면, 더 빠르고 정확한 결과를 얻을 수 있습니다.

## 하우 투
표준 오류에 쓰기를 하는 방법을 알아보겠습니다. C++에서는 `<iostream>` 헤더 파일을 사용하여 `std::cerr` 객체를 생성할 수 있습니다. `std::cerr` 객체는 표준 오류 스트림으로, 에러 메시지를 출력하는 데 사용됩니다.

아래의 예제 코드를 살펴보세요.

```C++
#include <iostream>

int main() {
    int num = 5;
    if (num < 10) {
        // 에러 메시지를 표준 오류에 쓰기
        std::cerr << "에러: 숫자는 10보다 작습니다." << std::endl;
    }
    return 0;
}
```
예제 코드를 실행하면, `에러: 숫자는 10보다 작습니다.`라는 메시지가 컴파일러에서 나타날 것입니다. 이 메시지는 프로그램에서 `num` 변수의 값이 10보다 작을 때 출력되는 것입니다.

## 딥 다이브
표준 오류에 쓰기를 하는 법을 더 자세히 알아보겠습니다. `std::cerr` 객체는 `std::ostream` 클래스의 인스턴스입니다. 이러한 클래스는 `<<` 연산자를 사용하여 출력을 처리하며, 이를 사용하여 표준 오류에 메시지를 쓸 수 있습니다.

또한, `std::cerr`는 `printf()` 함수처럼 포맷 스트링을 지원합니다. 이를 활용하면 더 다양한 형태의 메시지를 출력할 수 있습니다.

마지막으로, `std::cerr`는 버퍼링을 하지 않습니다. 따라서 프로그램이 중단되지 않고 바로 메시지를 출력할 수 있습니다.

## 이와 비슷한 주제
- [C++ 입출력 함수에 대한 개념](https://www.fun-coding.org/c++_I_O.html)
- [표준 입출력 전처리자와 입출력 포맷](https://modoocode.com/240)
- [표준 에러 이해하기](https://minjoosoo.github.io/2018-03-21/stderr/)
- [C++ 표준 라이브러리 문서](https://ko.cppreference.com/w/cpp)

## 참고자료
- [C++ 입출력 함수에 대한 개념](https://www.fun-coding.org/c++_I_O.html)
- [C++ 입출력 함수 예제 코드](https://modoocode.com/239)
- [C++ 입출력 전처리자와 입출력 포맷](https://modoocode.com/240)
- [표준 에러 이해하기](https://minjoosoo.github.io/2018-03-21/stderr/)
- [C++ 표준 라이브러리 문