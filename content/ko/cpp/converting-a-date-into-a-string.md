---
title:                "C++: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

우리는 종종 프로그래밍에서 날짜를 문자열로 변환하는 작업을 수행해야 합니다. 이는 날짜를 다른 형식으로 표시하고자 할 때, 예를 들어 특정한 형태의 웹사이트에서 요구하는 형식으로 날짜를 표시하고자 할 때 유용합니다. 이러한 상황에서 날짜를 문자열로 변환할 수 있는 방법을 알고 있다면 더 유연하고 다양한 프로그래밍을 할 수 있을 것입니다.

## 어떻게 하나요?

아래의 코드는 C++에서 날짜를 문자열로 변환하는 방법을 보여줍니다. 코드를 읽기 전에 "```C++" 및 "```" 사이의 코드 블록에 대한 이해가 있다고 가정합니다. 

```
#include <iostream>
#include <string>
#include <ctime>

using namespace std;

int main() {

  // 오늘 날짜를 가져옵니다.
  time_t rawtime;
  time(&rawtime);

  // 날짜를 문자열로 변환합니다.
  string dateString = ctime(&rawtime);

  // 출력 결과: Mon Sep 14 14:21:32 2020
  cout << dateString;

  return 0;
}
```

위 코드를 실행하면 오늘 날짜를 출력하게 됩니다. 우리는 ctime 함수를 사용하여 time_t 형식으로 날짜를 가져오고, 이를 다시 string 형식으로 변환합니다. 이를 통해 우리는 더 간단하게 날짜를 표현할 수 있으며, 원하는 형식으로 날짜를 출력할 수 있습니다.

## 깊이 파고들기

실제로 날짜를 문자열로 변환하는 과정은 이렇게 간단하지 않습니다. 우리는 다양한 다른 날짜 형식이 있기 때문에 이러한 형식에 맞추어 날짜를 변환해야 할 수도 있습니다. 또한, 다양한 언어로 날짜를 표시해야 할 경우 이를 지원하는 함수를 사용해야 합니다.

C++에서는 다양한 날짜 및 시간 관련 클래스를 제공하고 있어 우리가 원하는 날짜 형식으로 쉽게 변환할 수 있습니다. 예를 들어, std::put_time 함수를 사용하면 특정한 형식의 날짜를 출력할 수 있습니다. 또한, Boost 라이브러리를 사용하면 더 다양한 날짜 및 시간 관련 기능을 제공받을 수 있습니다.

## 참고 자료

- [C++ reference](https://en.cppreference.com/w/cpp/chrono/c/ctime)
- [Boost date_time library](https://www.boost.org/doc/libs/1_74_0/doc/html/date_time.html)
- [C++로 날짜 형식 변경하기](https://blockdmask.tistory.com/319)
- [C++에서 날짜 형식 활용하기](https://huskdoll.tistory.com/6)

## 참고하십시오

- [왜 C++를 배워야 할까요?](https://www.geeksforgeeks.org/why-learn-cpp/)
- [C++ 프로그래밍 언어 소개](https://developer.mozilla.org/ko/docs/Web/JavaScript/Introduction_to_Object-Oriented_JavaScript)
- [C++ 초급자를 위한 가이드](https://www.tutorialspoint.com/cplusplus/index.htm)