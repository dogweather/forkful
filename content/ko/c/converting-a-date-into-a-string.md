---
title:    "C: 날짜를 문자열로 변환하기"
keywords: ["C"]
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 과정이 왜 필요한지 궁금하지 않으신가요? C 프로그래밍을 할 때 여러분들은 날짜 데이터를 다루어야 하는 경우가 있을 수 있습니다. 이럴 때 날짜를 문자열로 변환하는 것은 데이터를 더 직관적으로 이해할 수 있게 해주기 때문입니다.

## 어떻게

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 현재 시간을 구하는 코드
    time_t curr_time;
    time(&curr_time);

    // 날짜를 문자열로 변환하는 과정
    char date_string[100];
    struct tm * time_info;
    time_info = localtime(&curr_time);
    strftime(date_string, sizeof(date_string), "%A, %B %d, %Y", time_info);

    // 출력
    printf("%s", date_string);

    return 0;
}

// 결과
// Monday, November 01, 2021
```

위 예제 코드에서 우리는 `strftime()` 함수를 사용하여 날짜를 원하는 형식대로 문자열로 변환할 수 있습니다. `strftime()` 함수는 시간 정보를 나타내는 `struct tm` 구조체와 변환하고자 하는 형식을 인자로 받아서 문자열로 변환해 줍니다. 변환할 수 있는 형식은 여러 가지가 있으며 [공식 문서](https://www.cplusplus.com/reference/ctime/strftime/)를 참고해 주시기 바랍니다.

## 깊이 들어가기

`strftime()` 함수를 이용하여 날짜를 문자열로 변환하려면 `struct tm` 구조체의 멤버 변수를 적절한 형식으로 작성해 주어야 합니다. 예를 들어 `%B`를 사용하면 월을 영문으로 출력해 줍니다. 만약 한국어로 출력하고 싶다면 `setlocale(LC_ALL, "ko_KR.UTF-8")`와 같이 `setlocale()` 함수를 사용하여 로케일을 설정해 주어야 합니다.

또한 `strftime()` 함수를 사용하여 현재 시간 뿐만 아니라 지정한 날짜의 문자열을 출력할 수도 있습니다. 해당하는 날짜를 `struct tm` 구조체에 저장한 뒤 파라미터로 넘겨주면 됩니다.

## 참고 자료

- [strftime() 함수 문서](https://www.cplusplus.com/reference/ctime/strftime/)
- [setlocale() 함수 문서](https://www.cplusplus.com/reference/clocale/setlocale/)