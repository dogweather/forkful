---
title:                "Arduino: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것에 참여하는 이유는 무엇일까요? 많은 사람들에게 새로운 것을 배우는 것은 혼자서 해결하기 어렵기 때문에 도전적이고 흥미롭습니다. 아두이노 프로그래밍을 통해 날짜 계산을 배우는 것은 최신 기술을 익히는 동시에 새로운 언어와 도구를 탐험하는 재미있는 방법입니다.

## 어떻게

아두이노를 사용하여 날짜를 계산하는 방법은 아주 간단합니다. 먼저 핵심 라이브러리인 "DateTime.h"를 추가하고 날짜를 입력받아 아두이노에서 몇 일 후의 날짜를 계산하는 코드를 작성하면 됩니다. 이 코드를 "```Arduino ... ```" 코드 블록 안에 작성하여 쉽고 빠르게 확인할 수 있습니다.

```Arduino
#include <DateTime.h>

DateTime now = DateTime(2021, 10, 5); // 오늘의 날짜를 입력하고,
int days = 30; // 계산하고 싶은 일수를 입력합니다.

DateTime futureDate = now + TimeSpan(days);

Serial.println("오늘은 " + now.asString() + "입니다.");
Serial.println(days + "일 후의 날짜는 " + futureDate.asString() + "입니다.");
```

위의 코드를 실행하면 오늘 날짜와 30일 후의 날짜가 모두 출력됩니다. 여러분은 이 코드를 수정하여 다양한 날짜를 계산해볼 수 있습니다.

## 깊이 파고들기

날짜를 계산하는 것은 그 자체로도 흥미로운 주제입니다. 더 나아가 우리가 사용하는 아주 간단한 코드가 실제로 어떻게 작동하는지 알아보는 것 또한 흥미롭습니다. "DateTime.h" 라이브러리의 소스 코드를 열어보면 날짜를 계산하기 위해 일어나는 일들을 자세히 볼 수 있습니다. 또한 라이브러리를 커스텀하여 더 많은 기능을 추가할 수도 있습니다.

## 참고

- [DateTime library 설명서 (영문)](https://playground.arduino.cc/Code/DateTime/)
- [DateTime library 소스 코드 (영문)](https://github.com/PaulStoffregen/DateTime)
- [Arduino 공식 홈페이지 (한글)](https://www.arduino.cc/)
- [아두이노 입문서 (한글)](https://www.instructables.com/id/%EC%95%84%EB%91%90%EC%9D%B4%EB%85%B8-%EC%9E%85%EB%AC%B8-%EC%84%9C%EA%B8%B0/)