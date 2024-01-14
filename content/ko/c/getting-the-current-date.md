---
title:                "C: 현재 날짜 가져오기"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

BLOG: C 프로그래밍의 날짜 함수에 대한 번역

## 왜

현재 날짜를 얻는 일은 프로그래밍을 하는 동안 상당히 자주 발생하는 일입니다. 예를 들어 데이터를 추적하거나 일정 기간 안에 특정 작업을 수행하는 데 유용한 작업입니다. C 프로그래밍에서 현재 날짜를 얻는 방법에 대해 알아보겠습니다.

## 대략적인 방법

C 프로그래밍에서 현재 날짜를 얻는 가장 간단한 방법은 `time()` 함수를 사용하는 것입니다. 이 함수는 시간 값이 들어 있는 변수를 반환합니다. 그 변수는 일반적으로 정수형 또는 long 형식으로 선언됩니다. 아래는 C 프로그래밍에서 날짜를 얻는 방법의 예제 코드입니다.

```C
#include <stdio.h>
#include <time.h>

int main()
{
  time_t time_now;
  time(&time_now);
  
  printf("현재 날짜 및 시간: %s", ctime(&time_now));
  
  return 0;
}
```

위 코드를 실행하면 다음과 같은 출력이 생성됩니다.

```bash
현재 날짜 및 시간: Mon Apr 12 18:00:00 2021
```

이 코드는 현재 날짜와 시간을 문자열 형식으로 반환합니다. `ctime()` 함수는 시간 값을 파라미터로 받아들이며, 이를 문자열로 변환하는 역할을 합니다.

## 심층 분석

앞에서 살펴본 코드는 현재 날짜와 시간을 문자열로 반환하는 간단한 예제입니다. 그러나 C 프로그래밍에서 날짜와 시간을 처리하는 더 복잡한 방법도 존재합니다. 일부 프로그래밍 과제에서는 숫자 형식이 아닌 다른 형식으로 날짜를 표현해야 할 때가 있습니다. 이 경우 `localtime()` 함수를 사용하여 `tm` 구조체를 반환하는 방법으로 날짜를 조작할 수 있습니다. 구조체를 사용해 숫자 값 대신 다른 형식의 날짜를 생성할 수 있습니다. 또한 `strftime()` 함수를 사용하면 더 다양한 형식의 날짜를 생성할 수 있습니다. 앞에서 사용한 `ctime()` 함수는 현재 날짜와 시간만 반환하지만, `strftime()` 함수는 날짜를 다양한 형식으로 포맷하여 반환할 수 있습니다. 더 많은 예제는 C 프로그래밍 문서 또는 온라인 리소스를 참조하십시오.

## 더 알아보기

- C 프로그래밍 문서: https://dojang.io/course/view.php?id=7
- C 날짜 및 시간 관련 함수: https://www.tutorialspoint.com/c_standard_library/time_h.htm
- strftime() 함수 문서: https://www.ibm.com/docs/en/zos/2.3.0?topic=functions-strftime-format-date-and-time-string