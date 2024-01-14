---
title:                "C++: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜: 

현재 날짜를 가져오는 것에 대해 궁금한 이유는 무엇일까요? C ++ 프로그래밍에서 현재 날짜를 사용할 수 있는 다양한 방법과 기능을 소개해드리겠습니다.

## 사용 방법:

먼저, `<ctime>` 헤더 파일을 포함하여 시간과 날짜 관련 함수를 사용할 수 있도록 해야합니다. 그런 다음 `time()` 함수를 사용하여 현재 시간이 포함된 변수를 선언합니다. 마지막으로, `asctime()` 함수를 사용하여 현재 날짜를 출력합니다.

```C++
#include <iostream>
#include <ctime>

int main() {
    time_t now = time(0); // 현재 시간과의 차이를 계산하는 변수 선언
    char* currentTime = asctime(localtime(&now)); // 현재 날짜 출력
    std::cout << "현재 날짜: " << currentTime << std::endl;
    return 0;
}
```

**출력 결과:**

```
현재 날짜: Tue Jan 26 16:32:19 2021
```

## 깊이 파고들기:

인터넷 검색을 통해 더 다양한 방식으로 현재 날짜를 얻는 방법을 알아볼 수 있습니다. 예를 들어, `chrono` 헤더 파일을 사용하여 보다 정교하게 현재 날짜를 처리할 수 있습니다. 또한 시간대에 대해 고려해야하는 경우 `Time Zone Database`를 사용할 수 있습니다.

## 관련 링크:

- [C++ Reference - time() 함수](https://en.cppreference.com/w/cpp/chrono/time_point)
- [GeeksforGeeks - C++로 현재 날짜 및 시간 가져오기](https://www.geeksforgeeks.org/how-to-get-current-date-and-time-in-c/)
- [haullie - C++로 시간대 관리하기](https://haullie.wordpress.com/2014/01/13/how-to-handle-time-zone-in-cpp/)