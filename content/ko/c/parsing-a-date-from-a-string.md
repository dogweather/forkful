---
title:                "문자열에서 날짜 추출하기"
html_title:           "C: 문자열에서 날짜 추출하기"
simple_title:         "문자열에서 날짜 추출하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

날짜 문자열을 분석하는 것은 날짜를 문자열 형식에서 컴퓨터가 이해할 수 있는 형식으로 변환하는 것입니다. 프로그래머들은 이를 하는 이유는 다양한 데이터 형식을 다루는 프로그램을 개발할 때 자주 마주치게 되는 작업이기 때문입니다.

## 방법:

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

int main()
{
    char date_string[] = "2021/05/01";
    struct tm tm;
    if (strptime(date_string, "%Y/%m/%d", &tm) == NULL) {
        printf("Invalid date format");
        return 1;
    }
    time_t t = mktime(&tm);
    printf("Parsed date: %s", ctime(&t));
    return 0;
}
```
출력:
```
Parsed date: Sat May 1 00:00:00 2021
```
예제 코드는 먼저 문자열 형식의 날짜를 가진 변수를 선언하고, `strptime` 함수를 사용하여 이를 `struct tm` 구조체로 변환합니다. 변환 과정에서 `"%Y/%m/%d"` 형식으로 날짜를 패턴화하는 것에 유의해야 합니다. 변환된 데이터는 `mktime` 함수를 사용하여 `time_t` 형태로 변환하고, 마지막으로 `ctime` 함수를 사용하여 문자열 형태로 출력합니다.

## 깊이 파고들기:

먼저, 날짜를 날짜 문자열에서 파싱하는 과정은 컴퓨터가 발명되기 전에도 있었던 문제로, 사람들 사이에서 약속된 포맷이 없어 매번 다른 형식으로 표현되는 기술적인 어려움이 있었습니다. 이를 보완하기 위해 ISO (International Organization for Standardization)는 8601 표준을 제정하여 날짜 표현 포맷을 통일하였습니다. 하지만 여전히 다양한 표준이 존재하기 때문에 프로그래머들은 날짜를 다루는 것이 복잡하며, 이를 분석하는 과정은 항상 예상치 못한 오류를 발생시킬 수 있습니다.

`strptime` 함수 외에도 날짜를 분석하는 방법으로는 정규 표현식을 사용하는 방법도 있습니다. 이는 문자열에서 정해진 패턴을 찾아 해당 부분을 추출하는 것으로, 더욱 자유로운 형식의 날짜 문자열을 다룰 수 있습니다. 하지만 이 역시도 복잡한 정규 표현식을 작성해야 하기 때문에 다소 어려울 수 있습니다.

날짜 분석 기능은 C언어 뿐만 아니라 다른 프로그래밍 언어에도 포함되어 있으며, 각 언어마다의 구현 방법은 다릅니다. 이를 참고하여 적절한 방법을 선택하는 것이 중요합니다.

## 관련 자료:

- [ISO 8601 표준](https://www.iso.org/iso-8601-date-and-time-format.html): 날짜와 시간에 대한 표준을 확인할 수 있습니다.
- [strptime 함수 문서](https://www.man7.org/linux/man-pages/man3/strptime.3.html): `strptime` 함수의 자세한 사용법을 확인할 수 있습니다.
- [정규 표현식 문서](https://www.regular-expressions.info/): 정규 표현식에 대한 자세한 정보를 확인할 수 있습니다.