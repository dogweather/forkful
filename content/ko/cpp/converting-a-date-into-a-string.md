---
title:    "C++: 날짜를 문자열로 변환하기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

**왜**: *왜* 누군가가 날짜를 문자열로 변환하는 것에 참여하려 하는지에 대해 1-2 문장으로 설명합니다.

**어떻게**: "```C++ ... ```" 코드 블록 내부에 코딩 예제와 샘플 출력을 포함합니다.

해매거나 삽질하지 마세요, C++에서 날짜를 문자열로 변환하는 것은 간단합니다. 다음 코드를 확인하세요:

```C++
#include <iostream>
#include <ctime>

int main()
{
    // 현재 시간을 가져온 다음 문자열로 변환합니다.
    time_t currentDate = time(0);
    char *dateString = ctime(&currentDate);

    std::cout << "오늘의 날짜는 " << dateString << "입니다" << std::endl;

    return 0;
}
```

출력:

```
오늘의 날짜는 Wed Apr 14 21:03:17 2021입니다.
```

**깊은 해석**: 날짜를 문자열로 변환하는 것의 깊은 정보에 대해 설명합니다.

우리는 `ctime()`과 `gmtime()`과 같은 라이브러리 함수를 사용하여 C++에서 쉽게 날짜를 문자열로 변환할 수 있습니다. `time()` 함수는 현재 시간 값을 반환하며, `ctime()` 함수는 `time_t` 형식의 시간 값을 다음과 같이 문자열로 변환하여 반환합니다:

```
Wed Apr 14 21:03:17 2021
```

그러나 중요한 점은, 이 함수들은 현재 시스템에 설정된 지역 설정을 반영하기 때문에 결과 문자열이 지역 설정에 따라 다를 수 있다는 것입니다. 따라서 가능하다면, `std::tm` 구조체를 사용하여 시간 값을 로컬 시간대로 변환한 다음 `std::strftime()` 함수를 사용하여 원하는 형식의 문자열로 변환하는 것이 좋습니다.

**참고**: 

**여기** 데이터 입출력 실습을 위한 기초적인 C++ 문법을 살펴보세요: https://www.geeksforgeeks.org/data-types-in-c

**여기** `ctime` 라이브러리 함수에 대해 자세히 알아보세요: https://www.cplusplus.com/reference/ctime/

**여기** 다양한 날짜와 시간을 다루는 방법을 알아보세요: https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm