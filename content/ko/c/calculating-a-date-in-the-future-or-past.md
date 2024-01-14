---
title:                "C: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜
이 글에서는 미래나 과거의 날짜를 계산하는 이유를 다룰 것입니다. 프로그래밍에서 날짜를 계산하는 것은 유용한 도구로써 많은 사람들이 사용하기 때문입니다. 

## 사용 방법
우리는 ```C``` 프로그래밍 언어를 사용하여 날짜를 계산하는 방법을 살펴볼 것입니다. 아래의 예제 코드와 함께 출력 결과도 제공하겠습니다. 

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
  // 현재 날짜와 시간을 얻어옵니다
  time_t now = time(NULL);
  struct tm *current_date = localtime(&now);

  // 미래의 날짜를 계산하는 예제
  current_date->tm_mday += 10;
  mktime(current_date);
  printf("10일 후의 날짜: %d월 %d일\n", current_date->tm_mon+1, current_date->tm_mday);

  // 과거의 날짜를 계산하는 예제
  current_date->tm_year -= 1;
  mktime(current_date);
  printf("1년 전의 날짜: %d월 %d일\n", current_date->tm_mon+1, current_date->tm_mday);

  return 0;
}
```

위의 코드에서 우리는 ```time.h``` 라이브러리를 사용하여 현재 날짜와 시간을 얻어오고, ```localtime()``` 함수를 사용하여 구조체에 저장하였습니다. 그리고 ```tm_mday``` 변수를 수정하여 미래나 과거의 날짜를 계산하며, ```mktime()``` 함수를 사용하여 수정한 날짜를 다시 계산하고 출력합니다. 이 예제에서는 10일 후의 날짜와 1년 전의 날짜를 계산하였습니다. 

## 깊게 들어가기
날짜를 계산하는 것은 사실 매우 복잡한 작업입니다. 예를 들어, 윤년(leap year)이나 윤달(leap month)과 같은 변수들을 고려해야 합니다. 이런 변수들은 날짜를 계산하는 데 있어서 큰 영향을 미치므로 정확하게 처리해주어야 합니다. 또한, 우리가 사용하는 운영체제와 C 언어의 버전에 따라서도 다른 결과를 얻을 수 있습니다. 이러한 이유로 날짜를 계산할 때는 항상 유의하고 주의해야 합니다. 

## 더 학습하기
- [C 언어 공식 문서](https://www.gnu.org/software/libc/manual/html_node/Time-Arithmetic.html): C 언어에서 제공하는 시간과 날짜 계산 기능에 대한 공식 문서입니다. 
- [날짜를 계산하는 방법](https://www.calculator.net/time-calculator.html): 간단한 온라인 도구를 사용하여 날짜와 시간을 계산하는 법에 대해 알아볼 수 있습니다. 
- [날짜와 시간 처리를 위한 코드 예제](https://www.programiz.com/c-programming/c-date-time-timestamp): C 언어로 날짜와 시간을 다루기 위한 코드 예제를 제공하는 사이트입니다. 

## 참고
- [Markdown 사용법](https://ko.wikipedia.org/wiki/마크다운): 위의 코드 블록과 같이 진하게 표시된 텍스트를 작성하기 위해 사용한 마크다운 문법에 대해 알아볼 수 있습니다.