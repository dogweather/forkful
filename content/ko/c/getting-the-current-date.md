---
title:    "C: 오늘 날짜 가져오기"
keywords: ["C"]
---

{{< edit_this_page >}}

## 왜

현재 날짜를 가져오는 것은 프로그래밍에서 매우 유용한 작업입니다. 예를 들어, 사용자가 앱이나 웹사이트를 사용할 때 현재 날짜를 표시하는 것은 사용자의 경험을 향상시키는 데 도움이 될 수 있습니다. 또는 특정 일자를 입력받아 이를 처리하는 프로그램을 만들 경우에도 현재 날짜를 가져오는 것은 필수적입니다.

## 방법

현재 날짜를 가져오는 가장 간단한 방법은 C 프로그래밍 언어의 time 라이브러리를 사용하는 것입니다. 아래는 time 라이브러리를 활용하여 현재 날짜를 출력하는 예제 코드입니다.

```C
#include <time.h>
#include <stdio.h>

int main(){
    // 현재 시간을 가져오는 time 함수의 파라미터는 NULL로 설정하면 됩니다.
    time_t currentTime = time(NULL); 

    // 받아온 시간을 struct tm 구조체로 변환하여 저장합니다.
    struct tm *date = localtime(&currentTime);

    // 변환된 시간 중에서 원하는 값을 printf 함수를 이용해 출력합니다.
    printf("현재 날짜: %d/%d/%d", date->tm_year + 1900, date->tm_mon + 1, date->tm_mday);

    return 0;
}
```

위 코드의 결과는 다음과 같습니다.

```
현재 날짜: 2021/5/7
```

## 깊이 파헤치기

time 라이브러리를 사용하여 현재 날짜를 가져오는 과정은 다음과 같습니다.

1. 먼저 time 함수를 사용하여 현재 시간을 초(second) 단위로 저장합니다. 이때 파라미터로 넘기는 값은 NULL로 설정합니다.
2. 초 단위로 저장된 현재 시간을 localtime 함수를 통해 사람이 읽을 수 있는 형식이 된 시간대로 변환합니다. 이때 변환된 시간은 struct tm 구조체에 저장됩니다.
3. 변환된 시간 중에서 원하는 값을 printf 함수를 이용해 출력합니다. 시간 관련 정보는 각각 tm_year, tm_mon, tm_mday 등의 변수에 담겨 있습니다.
4. 출력 결과는 현재 시스템의 설정에 따라 다양한 형식으로 표시될 수 있습니다. 예를 들어, 운영체제나 컴파일러의 설정에 따라 날짜가 "년/월/일"이 아니라 "월/일/년"으로 표시될 수 있습니다. 따라서 위 예제 코드는 참고용으로만 사용하시고 실제 프로젝트에서는 현재 시스템의 설정을 확인하여 적절한 출력 코드를 작성해야 합니다.

## 또 다른 내용

- [time 라이브러리 관련 문서](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [struct tm 구조체 관련 문서](https://www.tutorialspoint.com/c_standard_library/ctime.htm)
- [C 프로그래밍 관련 블로그](https://velog.io/@manusimple/series/C-Tutorial)