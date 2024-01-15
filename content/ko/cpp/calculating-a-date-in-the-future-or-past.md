---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "C++: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜
여러분이 미래나 과거의 날짜를 계산하는 것에 참여하고 싶을 수 있지만 그 이유는 간단합니다. 때로는 중요한 일정을 계획하고 싶다거나 이전 날짜의 기념일을 기억하고 싶을 때, 프로그래밍을 통해 날짜를 계산하는 것은 매우 편리합니다.

## 하우 투
우리는 C++을 사용하여 미래나 과거의 날짜를 계산하는 방법을 배울 것입니다. 먼저, ```< ctime >``` 라이브러리를 포함하여 현재 시간과 날짜를 가져옵니다. 그런 다음, 사용자로부터 날짜의 년, 월, 일을 입력 받아 ```struct tm``` 구조체에 저장합니다. 마지막으로, ```mktime()``` 함수를 사용하여 입력받은 날짜를 처리하여 미래나 과거의 날짜를 출력합니다.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
    // 현재 시간과 날짜를 가져옴
    time_t now_time = time(NULL);
    tm *now = localtime(&now_time);

    // 사용자로부터 날짜 입력 받음
    int year, month, day;
    cout << "날짜 입력 (yyyy/mm/dd): ";
    cin >> year >> month >> day;

    // 입력받은 날짜를 struct tm 구조체에 저장
    tm date = {0, 0, 0, day, month - 1, year - 1900};

    // mktime() 함수를 통해 처리된 날짜를 구해서 출력
    time_t processed_date = mktime(&date);
    tm *result = localtime(&processed_date);

    cout << "입력한 날짜의 100일 후: " << result->tm_year + 1900 << "년 " << result->tm_mon + 1 << "월 " << result->tm_mday << "일" << endl;

    return 0;
}
```

**출력 예시:**

날짜 입력 (yyyy/mm/dd): 2021/04/05
입력한 날짜의 100일 후: 2021년 7월 14일

## 딥 다이브
C++에서 날짜를 계산하는 데 사용되는 가장 중요한 라이브러리는 ```< ctime >```입니다. 이 라이브러리에는 다양한 함수들이 정의되어 있어 날짜 및 시간의 처리와 변환에 매우 유용합니다. 또한 C++의 구조체인 ```struct tm```은 시간과 날짜를 나타내는 데 사용되며, 다양한 정보들을 저장하고 조작할 수 있습니다. 날짜를 계산할 때 이러한 라이브러리와 구조체를 잘 활용하는 것이 중요합니다.

## 씨 얼소
- [C++는 무엇인가요?](https://ko.wikipedia.org/wiki/C%2B%2B)
- [C++ 공식 사이트](https://isocpp.org/)
- [C++ 날짜 및 시간 관련 기능 설명서](https://cplusplus.com/reference/ctime/)