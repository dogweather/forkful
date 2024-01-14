---
title:                "C: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 왜

날짜를 문자열로 변환하는 것이 중요한 이유입니다. 날짜를 문자열로 변환하면 다양한 형식으로 표시할 수 있으며, 다양한 데이터 형식에서 사용할 수 있습니다.

# 방법

```C
#include <stdio.h>
#include <string.h>
#include <time.h>

int main() {
  // 현재 시간을 구합니다.
  time_t t = time(NULL);
  struct tm *now = localtime(&t);

  // 날짜를 문자열로 변환합니다.
  char date[20];
  strftime(date, sizeof(date), "%Y-%m-%d", now);

  // 출력합니다.
  printf("변환된 날짜: %s", date);

  return 0;
}
```

출력:

```bash
변환된 날짜: 2020-04-13
```

# 깊이 파고들기

날짜를 문자열로 변환하는 과정에서 가장 많이 사용되는 함수는 `strftime()`입니다. 이 함수는 현재 시간 구조체(`struct tm`)의 정보를 지정된 형식에 맞춰 문자열로 변환합니다. 이 형식은 다양한 옵션으로 조합할 수 있으며, 자세한 정보는 [공식 문서](https://www.cplusplus.com/reference/ctime/strftime/)를 참고하시기 바랍니다.

# 참고

- [C 언어 공식 문서](https://www.cplusplus.com/)
- [날짜와 시간을 다루는 C 프로그래밍 가이드](https://www.tutorialspoint.com/cprogramming/c_date_time.htm)
- [C11 언어 표준 문서](https://www.iso.org/standard/57853.html)