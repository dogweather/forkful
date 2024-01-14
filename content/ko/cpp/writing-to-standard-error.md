---
title:                "C++: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

표준 에러를 쓰는 것의 장단점에 대해 생각해 본 적이 있나요? C++ 프로그래밍에서 표준 에러를 사용하는 이유와 이에 대한 깊은 고찰을 살펴보겠습니다.

## 사용법

표준 에러에 대해 많은 것을 들어보았지만 실제로 어떻게 작성하는지 궁금하시죠? 아래의 예시 코드를 참고하여 간단한 프로그램을 만들어보세요.

```C++
#include <iostream>

int main() {
  // 1. cerr로 표준 에러 출력 부분 코딩하기
  std::cerr << "[표준 에러 출력 예제] 처음 접속할 때 발생한 오류입니다." << std::endl;

  // 2. 예시 입력 받기
  std::cout << "이름을 입력하세요: ";
  std::string name;
  std::cin >> name;

  // 3. 예시 출력
  std::cout << "안녕하세요, " << name << "님!" << std::endl;

  return 0;
}
```

### 예시 출력

```
[표준 에러 출력 예제] 처음 접속할 때 발생한 오류입니다.
이름을 입력하세요: John
안녕하세요, John님!
```

## 깊이 알아보기

표준 에러를 사용하는 이유는 무엇일까요? 표준 에러와 표준 출력의 차이점은 무엇일까요? 정확한 사용법과 함께 더 자세한 내용을 살펴보겠습니다.

### 표준 에러와 표준 출력의 차이점

C++에서는 총 3가지의 표준 스트림이 존재합니다.
1. `std::cin`: 표준 입력
2. `std::cout`: 표준 출력
3. `std::cerr`: 표준 에러

위에서 예시 코드에서 사용한 것처럼 `std::cerr`은 표준 출력과 달리 에러 메시지를 출력하기 위해 사용됩니다. `std::cout`은 일반적인 출력을 담당하며, `std::cerr`은 예외 처리나 오류 등을 출력하는 데 사용됩니다.

## See Also

- [C++ 문서 - cout, cin, cerr 사용법 및 예제](https://modoocode.com/224)
- [표준 입출력 스트림 포맷 - cout, cin, cerr](https://www.techrepublic.com/article/managing-io-streams-in-c/)