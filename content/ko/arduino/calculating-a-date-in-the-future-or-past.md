---
title:    "Arduino: 미래나 과거의 날짜 계산하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜

우리는 종종 미래나 과거의 특정 날짜를 계산하고 싶을 때가 있습니다. 예를 들어, 생일이나 휴일이 언제인지 알고 싶을 수 있습니다. 이런 경우에 아두이노 프로그래밍을 사용하면 쉽게 날짜를 계산할 수 있습니다.

# 어떻게하기

아두이노에서는 내장된 시간 라이브러리를 사용하여 날짜를 쉽게 계산할 수 있습니다. 먼저 ```Time.h``` 라이브러리를 포함해야 합니다. 그런 다음 ```time_t``` 타입의 변수를 만들어 현재 시간을 저장합니다. 예를 들어:

```
#include <Time.h>

time_t now = time(NULL);
```

그런 다음 ```hour()```, ```minute()```, ```day()``` 등의 함수를 사용하여 변수에서 필요한 정보를 구할 수 있습니다. 아래는 현재 날짜와 시간을 계산하는 예제입니다.

```
#include <Time.h>

time_t now = time(NULL);

int hour = hour(now);
int minute = minute(now);
int day = day(now);

Serial.print("The current time is: ");
Serial.print(hour);
Serial.print(":");
Serial.print(minute);
Serial.print(", on day ");
Serial.println(day);
```

출력은 다음과 같이 나올 것입니다:

```
The current time is: 15:30, on day 2
```

# 깊이 파고들기

더 깊이 들어가 보면, 아두이노에서는 1970년 1월 1일 이후 경과한 초로 시간을 측정합니다. 이를 Unix 시간이라고 합니다. 따라서 시간을 계산하고 싶다면 Unix 시간을 변환해야 할 필요가 있습니다. 또한, 아두이노에서는 내장된 RTC(Real-Time Clock) 모듈을 사용하면 보다 정확한 시간을 계산할 수 있습니다.

# 더 알아보기

이 글에서는 아두이노를 사용하여 날짜를 계산하는 방법을 간략하게 살펴보았습니다. 더 자세한 내용은 아래 링크들을 참고해주세요.

- [Time 라이브러리 문서](https://www.pjrc.com/teensy/td_libs_Time.html)
- [RTC 모듈 사용하기](https://blog.naver.com/PostView.nhn?blogId=atork5445&logNo=221519282953)