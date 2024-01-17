---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "C++: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 무슨 말이고 왜 그렇죠?

계산 날짜는 미래 또는 과거의 날짜를 계산하는 것을 뜻합니다. 프로그래머들은 이 작업을 수행하는 이유는 다양합니다. 예를 들어, 특정한 날짜 이후의 이벤트를 계획하기 위해서이거나, 특정 날짜의 데이터를 분석하기 위해서일 수 있습니다.

# 하는 법:

```C++
// 오늘 날짜를 기준으로 다음날의 날짜를 계산하는 예제입니다.
#include <iostream>
#include <ctime>

using namespace std;

int main() {
  // 현재 시간을 얻어옵니다.
  time_t now = time(0);
  // tm 구조체를 사용하여 현재 시간을 로컬 시간대로 변환합니다.
  tm* curr = localtime(&now);
  // 다음날의 날짜를 계산합니다.
  curr->tm_mday += 1;
  // tm 구조체를 다시 시간값으로 변환합니다.
  now = mktime(curr);
  // 타임스탬프를 문자열로 변환하여 출력합니다.
  cout << "다음날의 날짜는: " << ctime(&now);
  
  return 0;
}
```

출력:
```
다음날의 날짜는: Sun Dec 13 20:16:13 2020
```

# 깊이 아래 뜻:

(1) 과거부터 날짜를 계산하는 것은 역사적으로 중요한 문제입니다. 예를 들어, 로마 제국 시대에는 매년 새로운 달력을 만들어야 했고, 이를 위해 계산하는 알고리즘이 필요했습니다.
(2) 날짜를 계산하는 다른 방법으로는, C++의 내장 함수인 `mktime()`을 사용하는 것이 있습니다.
(3) 날짜를 계산하는 알고리즘의 구현 방식은 매우 다양하며, 다양한 프로그래밍 언어에서 다른 방식으로 구현되어있습니다.

# 연관된 소스:

- [C++ mktime() function](https://www.programiz.com/cpp-programming/library-function/ctime/mktime)
- [History of the Julian and Gregorian calendars](https://www.timeanddate.com/calendar/julian-gregorian-switch.html)