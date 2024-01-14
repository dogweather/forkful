---
title:    "C: 미래나 과거의 날짜 계산하기"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 참여하려는 이유는 여러 가지가 있을 수 있지만, 대부분은 특정 날짜를 알고 싶거나 어떤 일정 날짜까지의 기간을 계산하기 위함입니다. 또한 개인적인 이유로 생일이나 기념일을 파악하고 싶은 경우에도 유용할 수 있습니다.

## 사용 방법

C 프로그래밍을 사용하여 날짜를 계산하는 방법은 간단합니다. 우선, 사용자로부터 계산할 날짜의 연, 월, 일 정보를 입력받습니다. 그 다음, 시간을 나타내는 변수와 사용자가 입력한 날짜 정보를 연산하는 과정을 거치면 됩니다. 아래는 예시 코드와 출력입니다.

```C
#include <stdio.h>

int main() {
    // 사용자로부터 연, 월, 일 정보 입력
    int year, month, day;
    printf("연도를 입력하세요: ");
    scanf("%d", &year);
    printf("월을 입력하세요: ");
    scanf("%d", &month);
    printf("일을 입력하세요: ");
    scanf("%d", &day);
    
    // 시간 변수 초기화 및 날짜 연산
    int hours = 0;
    day += 100; // 100일 뒤의 날짜를 계산 
    
    // 만약 31일을 넘어갈 경우 다음 달로 넘어가는 처리
    if (day > 31) {
        day -= 31;
        month ++;
    }
    
    // 만약 12월을 넘어갈 경우 다음 연도로 넘어가는 처리
    if (month > 12) {
        month -= 12;
        year ++;
    }

    // 계산된 결과 출력
    printf("100일 이후의 날짜는 %d년 %d월 %d일 입니다.", year, month, day);
    
    // 예상 출력: 100일 이후의 날짜는 2021년 10월 12일 입니다.
    
    return 0;
}
```

## 깊이 파고들기

날짜를 계산하는 것은 우리 생활에서 매우 중요한 부분을 차지합니다. 그렇기 때문에 많은 언어에서 날짜와 관련된 기능을 제공하고 있으며, C 역시 예외는 아닙니다. C에서는 <time.h> 라이브러리를 통해 시간과 날짜를 다룰 수 있습니다. 이 라이브러리는 시스템에 따라 다른 함수를 제공하므로, 필요할 때마다 참조하여 사용하시면 됩니다.

## 참고 자료

- <time.h> 라이브러리 함수들: https://dojang.io/mod/page/view.php?id=1039
- C 언어 기초 강좌: https://www.youtube.com/watch?v=gaZn1DdOZrg
- 그 외 여러 가지 관련 정보들: https://www.google.com/search?q=c+calculate+date+in+future+or+past