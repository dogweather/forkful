---
title:                "테스트 작성하기"
html_title:           "C++: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍을 하다보면 반복적인 코드를 어떻게든 줄이려고 노력하게 됩니다. 이럴 땐 테스트를 적절히 작성하여 일일이 코드를 디버깅하고 수정하는 시간과 노력을 절약할 수 있습니다.

## 작성 방법

아래는 간단한 C++ 코드 예시와 그에 따른 결과물입니다. 여러분도 이런 식으로 테스트를 작성하고 실행해보세요!

```C++
#include <iostream>
#include <string>

using namespace std;

// 테스트를 작성할 함수 정의
int addNumbers(int a, int b) {
    return a + b;
}

// 각각의 테스트는 함수 앞에 CHECK 함수를 사용하여 테스트 이름과 기대 결과값을 명시합니다.
CHECK("더하기 테스트") {
    // 테스트 할 코드
    int result = addNumbers(3, 5);

    // 기대 결과값과 실제 결과값 비교
    CHECK_EQUALS(result, 8);
}

CHECK("길이 비교 테스트") {
    string str = "Hello";
    
    // 테스트 할 코드
    int length = str.length();
    
    CHECK_EQUALS(length, 5);
}

// 모든 테스트를 한번에 실행하기
bool result = runTests();

// 실행 결과에 따른 메시지 출력
if (result) {
    cout << "모든 테스트가 성공하였습니다!" << endl;
} else {
    cout << "테스트 중 실패한 항목이 있습니다." << endl;
}
```

## 깊게 파보기

위의 예시는 함수의 반환값과 비교하는 간단한 테스트였지만, 테스트할 대상이 객체일 경우에는 MEMBER_CHECK 함수를 사용하여 객체의 멤버 변수를 비교할 수도 있습니다. 또한, 테스트를 작성할 때 함수와 변수의 이름을 명확하게 짓는 것이 중요합니다. 이러한 작은 노력으로 나중에 코드를 읽거나 수정할 때 효율적으로 테스트를 활용할 수 있습니다.

## 참고 자료

- [C++의 단위 테스트 작성하기](https://docs.microsoft.com/ko-kr/visualstudio/test/writing-unit-tests-for-c-cpp?view=vs-2019)
- [Google Test: C++ 테스트 프레임워크](https://github.com/google/googletest)

## 더 읽어보기

- [테스트 주도 개발(Test-driven development)](https://ko.wikipedia.org/wiki/%ED%85%8C%EC%8A%A4%ED%8A%B8_%EC%A3%BC%EB%8F%84_%EA%B0%9C%EB%B0%9C)
- [반복적 탐색 조건문을 대체할 테스트 작성하기](https://docs.microsoft.com/ko-kr/dotnet/csharp/tutorials/guessing-game/test-branch-with-tests?view=netframework-4.8)
- [테스트 커버리지(Test coverage)](https://ko.wikipedia.org/wiki/%ED%85%8C%EC%8A%A4%ED%8A%B8_%EC%BB%A4%EB%B2%84%EB%A6%AC%EC%A7%80)