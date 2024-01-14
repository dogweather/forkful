---
title:    "C: 두 날짜의 비교"
keywords: ["C"]
---

{{< edit_this_page >}}

## 왜

날짜를 비교하는 것에 대해 생각해 본 적이 있나요? C 프로그래밍에서 날짜 비교는 매우 유용한 기술입니다. 이 기술을 사용하면 프로그램이 특정 날짜를 참조하거나 다음 날짜를 계산하는 등 많은 작업을 할 수 있게 됩니다.

## 사용 방법

우리가 비교할 두 날짜를 정의하는 데는 여러 가지 방법이 있습니다. 그 중 가장 일반적인 방법은 "struct tm" 구조체를 사용하는 것입니다. 이 구조체에는 년, 월, 일, 시간 등 날짜와 관련된 정보를 저장할 수 있게 해줍니다.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // 비교할 두 날짜를 정의합니다.
    struct tm first_date = { .tm_year = 2021, .tm_mon = 1, .tm_mday = 1 };
    struct tm second_date = { .tm_year = 2020, .tm_mon = 12, .tm_mday = 31 };
    
    // difftime() 함수를 사용하여 두 날짜 간의 차이를 계산합니다.
    double diff = difftime(mktime(&first_date), mktime(&second_date));
    
    // 결과를 출력합니다.
    printf("두 날짜 간의 차이는 %f초입니다.", diff);
    
    return 0;
}
```

위의 코드는 "difftime()" 함수를 사용하여 두 날짜 간의 차이를 계산하고, 결과를 출력하는 간단한 예시입니다. 실행하면 결과는 "두 날짜 간의 차이는 86400초입니다."라는 메시지를 출력할 것입니다. 이는 2021년 1월 1일과 2020년 12월 31일 사이의 차이가 1일이므로 86400초(1일)가 계산된 것입니다.

## 깊이 파고들기

날짜를 비교하는 것은 실생활에서도 매우 일반적인 일이지만, 컴퓨터에서는 다루기가 조금 더 까다로울 수 있습니다. 이는 컴퓨터가 일반적으로 날짜를 "epoch"이라는 기준점으로부터 지난 시간(초)으로 저장하기 때문입니다.

따라서 우리가 컴퓨터에서 사용하는 날짜 비교 기술은 크게 두 가지로 구분할 수 있습니다. 하나는 "struct tm"을 사용하는 방법이고, 다른 하나는 "time_t"를 사용하는 방법입니다. "struct tm" 방식은 위에서 설명한 대로 날짜와 관련된 정보를 하나씩 따로 저장하는 방식이며, "time_t" 방식은 "epoch"로부터 지난 시간을 초 단위로 저장하는 방식입니다.

실제로 "difftime()" 함수를 사용할 때도 시간 차이를 계산하려는 두 날짜 중 하나를 "epoch"로 바꾸어야 한다는 점이 특징이라고 볼 수 있습니다.

## 참고자료

- [C 언어를 이용한 날짜와 시간 다루기 | 코딩팩토리](https://coding-factory.tistory.com/250)
- [struct tm 구조체 문서 | Cplusplus.com](http://www.cplusplus.com/reference/ctime/tm/)
- [time_t 구조체 문서 | Cplusplus.com](http://www.cplusplus.com/reference/ctime/time_t/)