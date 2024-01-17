---
title:                "날짜를 문자열로 변환하는 방법"
html_title:           "C: 날짜를 문자열로 변환하는 방법"
simple_title:         "날짜를 문자열로 변환하는 방법"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# 무엇이며 왜?

날짜를 문자열로 변환하는 것은 프로그래머가 날짜 데이터를 다룰 때 유용합니다. 날짜를 문자열로 변환하면 구분 가능하고 포맷팅된 텍스트 값으로 표현할 수 있습니다. 이는 날짜 데이터를 사용자에게 이해하기 쉽게 표현하기 위함이고 더 나은 사용자 경험을 제공하기 위한 것입니다.

# 어떻게?

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t t = time(NULL); // 현재 시간 정보를 얻어옴
  struct tm *tm = localtime(&t); // 시간 정보를 구조체로 변환
  
  // strftime 함수를 사용하여 날짜를 포맷팅된 문자열로 변환
  char date_string[20];
  strftime(date_string, sizeof(date_string), "%Y-%m-%d", tm);
  printf("오늘의 날짜는: %s\n", date_string);
  
  return 0;
}
```

출력:
```
오늘의 날짜는: 2021-07-05
```

# 깊게 들어가보기

(1) 날짜를 문자열로 변환하는 기능은 C 언어의 표준 라이브러리에 포함되어 있습니다. 따라서 별도의 라이브러리나 외부 도구를 사용하지 않고도 쉽게 구현할 수 있습니다.
(2) C 외에도 다른 프로그래밍 언어에서도 날짜를 문자열로 변환하는 기능을 제공합니다. 예를 들어, 파이썬에서는 datetime 모듈을 사용하여 날짜를 문자열로 변환할 수 있습니다.
(3) C 언어에서는 time_t와 struct tm 구조체를 사용하여 날짜를 다룹니다. 시간 정보는 초, 분, 시간, 일, 월, 연도 등의 필드로 구성되어 있으며, C 언어에서 제공하는 함수를 사용하여 이를 다룰 수 있습니다.

# 관련 자료 보기
https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm