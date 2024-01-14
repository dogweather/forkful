---
title:    "C++: 두 날짜 비교하기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜 두 개를 비교하는 것의 장단점에 대해 알아보기 위해 이 포스트를 읽으시는 독자들은 필요한 경우 프로그램에서 날짜를 비교해야할 때가 있을 것입니다.

## 어떻게

날짜를 비교하는 데에는 여러 가지 방법이 있지만, 이 포스트에서는 C++에서 가장 간단하게 사용할 수 있는 방법을 알려드리겠습니다. 아래 코드 예제를 통해 날짜를 조작하는 방법을 자세하게 알아보세요.

```C++
#include <iostream>
#include <ctime>

int main() {
  //날짜를 비교할 두 변수를 선언합니다.
  // 구조체 tm은 time.h 라이브러리에 정의되어 있습니다.
  struct tm date1 = {0, 0, 0, 1, 0, 2021 - 1900};  //2021년 1월 1일
  struct tm date2 = {0, 0, 0, 15, 0, 2021 - 1900}; //2021년 1월 15일

  // mktime 함수를 사용하여 날짜를 time_t 형식으로 변환합니다.
  // time_t는 1970년 1월 1일 이후로 경과한 시간을 초로 나타내는 정수형입니다.
  // 여기서는 비교를 위해 date1, date2를 모두 같은 시간으로 설정합니다.
  time_t t1 = mktime(&date1);
  time_t t2 = mktime(&date2);

  // 두 날짜를 비교합니다.
  if (difftime(t1, t2) > 0) {
    // date1이 date2보다 더 늦은 날짜일 경우
    std::cout << "date1 is a later date than date2" << std::endl;
  } else if (difftime(t2, t1) > 0) {
    // date2가 date1보다 더 늦은 날짜일 경우
    std::cout << "date2 is a later date than date1" << std::endl;
  } else {
    // 두 날짜가 같은 경우
    std::cout << "both dates are the same" << std::endl;
  }

  return 0;
}

//출력결과:
//date2 is a later date than date1
```

위 코드를 실행하면 비교하는 두 날짜 중 더 늦은 날짜를 출력합니다. difftime() 함수를 사용하여 비교를 해주었습니다. 이 함수는 두 날짜 사이의 시간 차를 계산해주는 함수입니다.

## Deep Dive

비교하는 날짜가 매우 많거나 복잡한 경우, difftime() 함수만으로는 충분하지 않을 수 있습니다. 이럴 때에는 C++에서 제공하는 다양한 날짜 비교 함수들을 적절하게 사용하면 더 세부적으로 날짜를 비교할 수 있습니다. 이러한 함수들을 활용하여 프로그램의 요구사항에 맞는 가장 최적화된 방법으로 날짜를 비교할 수 있습니다.

## See Also

- [C++에서의 날짜 비교](https://www.cplusplus.com/reference/ctime/)
- [difftime() 함수에 대한 설명](https://www.cplusplus.com/reference/ctime/difftime/)