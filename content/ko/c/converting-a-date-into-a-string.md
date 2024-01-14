---
title:                "C: 날짜를 문자열로 변환하기"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
날짜를 문자열로 변환하는 것이 왜 중요한지 궁금하셨나요? 프로그래밍에서 날짜를 문자열로 변환하는 이유는 많습니다. 예를 들어, 데이터베이스에서 날짜를 문자열로 쿼리하는 경우, 데이터베이스의 날짜 형식과 일치시켜야 합니다. 이를테면, 우리는 오늘의 날짜를 yyyy-mm-dd 형식에 맞춰서 문자열로 변환해야 합니다.

## 어떻게
날짜를 문자열로 변환하는 코드는 간단하지만, 언어마다 조금씩 다릅니다. 여기서는 C 프로그래밍 언어를 사용하겠습니다. 아래의 코드 블록을 사용하여 날짜를 문자열로 변환하는 예제를 살펴보세요!

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    // 오늘의 날짜를 가져옵니다.
    time_t now = time(NULL);

    // tm 구조체에 날짜를 저장합니다.
    struct tm *t = localtime(&now);

    // 날짜를 문자열로 변환합니다.
    char date[11];
    strftime(date, sizeof(date), "%Y-%m-%d", t);

    // 결과를 출력합니다.
    printf("오늘의 날짜는: %s", date);

    return 0;
}
```

위의 코드를 실행하면 다음과 같은 결과가 나타납니다:

```
오늘의 날짜는: 2021-10-12
```

친절한 독자분들을 위해 좀 더 자세한 설명을 드리자면, 위의 코드에서 우리는 time_t, struct tm, 그리고 strftime 함수를 사용했습니다. time_t는 초 단위로 오늘의 날짜와 시간을 나타내는 개념입니다. localtime 함수는 time_t 값을 주어진 지역의 시간대에 맞는 struct tm 값으로 변환해줍니다. 마지막으로 strftime 함수는 struct tm을 사용하여 원하는 형식으로 날짜를 문자열로 변환해줍니다.

## 깊이 파고들기
날짜를 문자열로 변환하는 것은 간단하지만, 이를 위해 사용하는 다양한 함수와 개념들이 있습니다. 이를 더 잘 이해하기 위해서는 날짜와 시간에 대한 더 많은 공부가 필요합니다. 또한 각 언어마다 날짜를 문자열로 변환하는 방식이 조금씩 다른데, 이를 사용할 때에는 반드시 그 언어의 문서를 참고하는 것이 좋습니다.

## 참고 자료
- [C 언어를 한 눈에 정리한 사이트](https://dojang.io/course/view.php?id=2)
- [C 언어에서 time_t와 struct tm을 사용하는 예제](https://modoocode.com/224)
- [strftime 함수에 대한 자세한 설명](https://www.cplusplus.com/reference/ctime/strftime/)
- [데이터베이스에서 날짜를 문자열로 쿼리하는 예제](https://www.bennadel.com/blog/190-querying-dates-in-mysql-using-date_format-or-str_to_date.htm)

## 더 읽어보기