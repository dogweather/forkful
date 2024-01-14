---
title:                "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 왜

현재 날짜를 알고 싶은 이유는 프로그래밍에서 매우 유용한 기능입니다. 예를 들어, 어떤 이벤트나 작업을 수행할 때 그 날짜를 자동으로 기록하거나 특정 날짜를 기준으로 동작을 제어하는 등의 많은 상황에서 사용될 수 있습니다.

# 사용 방법

먼저, 프로그램에서 날짜 관련 함수를 사용하기 위해 `time.h` 라이브러리를 `#include` 해야 합니다. 그리고 `time_t` 데이터 타입의 변수를 선언한 후 `time()` 함수를 통해 현재 날짜와 시간을 변수에 저장할 수 있습니다. 마지막으로 `localtime()` 함수를 사용하여 변수에 저장된 날짜를 원하는 포맷으로 변환할 수 있습니다.

```C
#include <time.h>

int main() {
    time_t now;
    time(&now); // 현재 날짜와 시간을 변수에 저장
    struct tm *local = localtime(&now); // 변수에 저장된 값을 원하는 포맷으로 변환
    printf("현재 날짜: %d년 %d월 %d일\n", local->tm_year + 1900, local->tm_mon + 1, local->tm_mday);

    return 0;
}
```

위의 코드는 현재 날짜를 `년-월-일` 형태로 출력합니다. `local` 변수의 멤버들을 사용하여 원하는 형식으로 날짜를 출력할 수 있으니 자유롭게 함께 실험해 보세요.

# 깊이 파고들기

현재 날짜를 얻는 더 세부적인 방법은 `time()` 함수와 `struct tm` 구조체의 멤버들을 직접 다루는 것입니다. 시스템 시간은 1970년 1월 1일부터 경과한 초 단위로 표현되기 때문에, 이를 변환하여 의미있는 값으로 사용할 수 있습니다. 자세한 내용은 아래 링크를 참고해 보세요.

# 관련 링크

- [ctime 라이브러리](https://ko.wikipedia.org/wiki/Ctime)
- [time.h 라이브러리](https://dojang.io/mod/page/view.php?id=259)
- [tm 구조체 구조 설명](https://modoocode.com/279)
- [시스템 시간과 유닉스 타임스탬프](https://zetawiki.com/wiki/%EC%8B%9C%EC%8A%A4%ED%85%9C_%EC%8B%9C%EA%B0%84%EA%B3%BC_%EC%9C%A0%EB%8B%89%EC%8A%A4_%ED%83%80%EC%9E%84%EC%8A%A4%ED%83%AC%ED%94%84)