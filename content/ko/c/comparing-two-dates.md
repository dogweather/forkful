---
title:                "두 날짜 비교하기"
html_title:           "C: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
날짜 두 개를 비교하는 것에 참여하는 이유는 간단합니다. 날짜는 소프트웨어에서 매우 중요한 역할을 합니다. 우리는 날짜를 비교하여 특정 이벤트가 언제 발생했는지 또는 더 이상 유효하지 않은 날짜인지 판단할 수 있습니다.

## 하는 방법
날짜를 비교하는 방법은 매우 간단합니다. 첫 번째 날짜와 두 번째 날짜를 변수에 할당한 다음, ```>``` 또는 ```<``` 식의 논리 연산자를 사용하여 비교하면 됩니다. 아래는 더 자세한 예제와 함께 코딩하는 방법입니다.

```c
#include <stdio.h>

int main(void) {
  // 첫 번째 날짜를 변수로 할당합니다.
  int month1 = 7;
  int day1 = 15;
  int year1 = 2021;

  // 두 번째 날짜를 변수로 할당합니다.
  int month2 = 7;
  int day2 = 16;
  int year2 = 2021;

  // 첫 번째 날짜가 두 번째 날짜보다 빠른지 비교합니다.
  if (year1 < year2 || (year1 == year2 && month1 < month2) || (year1 == year2 && month1 == month2 && day1 < day2)) {
    printf("%d/%d/%d is earlier than %d/%d/%d", month1, day1, year1, month2, day2, year2);
  } else {
    printf("%d/%d/%d is earlier than %d/%d/%d", month2, day2, year2, month1, day1, year1);
  }

  return 0;
}
```

위 코드는 두 날짜를 비교하여 더 이른 날짜를 출력하는 예제입니다. 만약 두 날짜가 같은 경우 두 번째 날짜를 출력합니다. 아래는 이 코드의 출력 예시입니다.

```
7/15/2021 is earlier than 7/16/2021
```

## 깊이 파고들기
두 날짜를 비교하는 것은 초보 프로그래머들이 자주 겪는 문제입니다. 이때 조심해야 할 것은 우리가 사용하는 날짜 시스템입니다. 우리가 사용하는 그레고리력은 모든 해가 같은 길이를 갖지 않기 때문에, 이를 고려하여 날짜를 비교해야 합니다. 또한 윤년을 계산하여 정확한 비교를 할 수 있도록 해야 합니다. 즉, 날짜를 비교할 때는 간단한 비교가 아닌 사용하는 날짜 시스템의 세부 사항을 고려해야 합니다.

## 또 다른 참고 자료
- [C 언어 공식 문서](https://en.cppreference.com/w/c)
- [컴퓨터 프로그래밍 연습 사이트](https://www.hackerrank.com/domains/c)