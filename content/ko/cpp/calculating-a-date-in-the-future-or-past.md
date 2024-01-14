---
title:    "C++: 미래나 과거의 날짜 계산하기"
keywords: ["C++"]
---

{{< edit_this_page >}}

## 왜

나중이나 과거의 날짜를 계산하는 것에 관심을 가지는 이유는 매우 다양합니다. 예를 들어, 생일이나 기념일을 계산하는 경우, 비즈니스 일정을 조정하는 경우 또는 여행 계획을 세우는 경우에도 날짜를 계산해야 할 수 있습니다. 이러한 상황에서 날짜 계산을 잘 다루는 것은 중요합니다.

## 어떻게

우리가 알고싶은 대부분의 날짜는 현재 날짜와의 차이로 계산됩니다. 따라서 현재 날짜를 알면 미래나 과거 날짜를 쉽게 계산할 수 있습니다. 다음은 C++로 날짜를 계산하는 간단한 예제 코드입니다.

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // 현재 날짜 가져오기
    time_t currentTime = time(NULL);
    tm* current = localtime(&currentTime);

    // 목표 날짜인 2020년 12월 25일 계산하기
    current->tm_year = 120; // 2020년 -> 1900년 기준으로 계산하므로 2020년 - 1900 = 120
    current->tm_mon = 11; // 12월 -> 0부터 시작하므로 12월 - 1 = 11
    current->tm_mday = 25; // 25일

    // 시간 정보 초기화
    current->tm_hour = 0;
    current->tm_min = 0;
    current->tm_sec = 0;

    // mktime 함수를 이용하여 날짜 계산하기
    time_t goalTime = mktime(current);
    tm* goal = localtime(&goalTime);

    // 계산 결과 출력
    cout << "2020년 12월 25일은 ";
    cout << goal->tm_year + 1900 << "년 ";
    cout << goal->tm_mon + 1 << "월 ";
    cout << goal->tm_mday << "일입니다." << endl;

    return 0;
}
```

출력 결과는 다음과 같습니다.

```
2020년 12월 25일은 2020년 12월 25일입니다.
```

위 예제 코드에서는 time 라이브러리의 tm 구조체를 사용하여 날짜를 계산하고 출력하였습니다. 이 외에도 chrono 라이브러리의 duration, time_point 클래스를 이용하여 시간을 쉽게 계산할 수 있습니다.

## 딥 다이브

위에서 예제로 사용한 mktime 함수는 현재 날짜와 시간 정보를 이용하여 지정한 날짜와 시간을 계산하여 리턴하는 함수입니다. 따라서 이 함수를 사용할 때에는 문제가 되는 시간 정보를 미리 초기화해주어야 합니다. 또한 time 라이브러리의 다양한 함수를 이용하여 날짜와 시간을 다룰 수 있습니다.

마지막으로, 위에서 사용한 localtime 함수는 현재 날짜와 시간을 미국 뉴욕 시간 기준으로 출력합니다. 따라서 사용자의 시스템이 다른 시간대를 사용하는 경우에는 결과가 다를 수 있습니다.

## 참고 자료

- [cppreference.com에서 현재 시간 정보 가져오기](https://cppreference.com)
- [time 라이브러리 사용하기](https://www.geeksforgeeks.org/c-program-print-current-day-date-time/)
- [chrono 라이브러리 사용하기](https://www.geeksforgeeks.org/working-with-date-and-time-in-cpp/)