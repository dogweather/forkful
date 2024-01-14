---
title:    "C: 현재 날짜 가져오기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜?

현재 날짜를 가져오기 위해 누군가가 관심을 가질 수 있는 이유는 다양합니다. 예를 들어, 파일에 저장하거나 프로그램의 로그를 기록하는 등의 목적으로 현재 날짜를 사용할 수 있습니다. 또는 특정 이벤트나 기능을 수행하기 위해 시간 체크가 필요할 수 있습니다. 어떤 이유든지 간에, 현재 날짜를 가져오는 것은 프로그래밍에서 매우 유용합니다.

## 어떻게?

C 프로그래밍에서 현재 날짜를 가져오려면, <time.h> 헤더 파일을 포함해야 합니다. 이후에는 `time()` 함수를 통해 현재 시간의 숫자 값을 얻을 수 있습니다. 아래의 예제 코드와 출력 결과를 통해 확인해봅시다.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 현재 시간의 숫자 값을 저장할 변수 선언
    time_t now;

    // 현재 시간 구하기
    time(&now);

    // 현재 날짜와 시간을 문자열 형태로 변환
    char* current_time = ctime(&now);

    // 출력
    printf("현재 날짜와 시간: %s", current_time);

    return 0;
}
```

출력 결과:

```
현재 날짜와 시간: Fri Jun 18 20:09:42 2021
```

## 딥 다이브

현재 날짜를 가져오는 `time()` 함수는 Unix 시간을 초 단위로 반환합니다. Unix 시간은 1970년 1월 1일 0시 0분 0초를 기준으로한 지난 시간의 초 단위로 표현된 값입니다. 따라서 이 값을 이용하여 다양한 날짜와 시간 정보를 얻을 수 있습니다.

또한, `localtime()` 함수를 사용하면 시스템의 현재 지역 시간을 구할 수 있습니다. 이 함수는 구조체를 반환하며, `tm` 구조체에는 날짜와 시간 정보가 저장되어 있습니다. 아래의 예제 코드를 통해 확인해봅시다.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 현재 시간의 숫자 값을 저장할 변수 선언
    time_t now;

    // 현재 시간 구하기
    time(&now);

    // 현재 지역 시간 구하기
    struct tm* current_time = localtime(&now);

    // 현재 날짜 및 시간 출력
    printf("현재 년도: %d\n", current_time->tm_year + 1900);
    printf("현재 달: %d\n", current_time->tm_mon + 1);
    printf("현재 일: %d\n", current_time->tm_mday);
    printf("현재 시간: %d\n", current_time->tm_hour);
    printf("현재 분: %d\n", current_time->tm_min);
    printf("현재 초: %d\n", current_time->tm_sec);

    return 0;
}
```

출력 결과:

```
현재 년도: 2021
현재 달: 6
현재 일: 18
현재 시간: 20
현재 분: 9
현재 초: 42
```

## 참고 자료

- [The C Programming Language Documentation: Time Functions](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Unix Time - Wikipedia](https://en.wikipedia.org/wiki/Unix_time)
- [ctime() vs. localtime() in C](https://stackoverflow.com/questions/3459150/ctime-vs-localtime-in-c)

## 참고 자료(en)

- [The C Programming Language Documentation: Time Functions](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Unix Time - Wikipedia](https://en.wikipedia