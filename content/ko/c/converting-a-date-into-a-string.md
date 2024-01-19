---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무슨 일이고 왜 하는가?
날짜를 문자열로 변환하는 것은 특정 날짜를 사람이 읽을 수 있는 텍스트 형태로 변환하는 프로그래밍 처리입니다. 프로그래머들은 이 작업을 통해 사용자에게 친숙한 형태로 날짜 정보를 표현하거나, 일반적인 문자열 연산을 적용하기 위해 합니다.

## 이렇게 해보세요:
다음은 C 언어를 사용한 날짜를 문자열로 변환하는 간단한 코드 예시입니다.

```C
#include <time.h>
#include <stdio.h>

int main() {
    char buffer[26];
    time_t timer;
    struct tm* tm_info;

    time(&timer);
    tm_info = localtime(&timer);

    strftime(buffer, 26, "%Y-%m-%d %H:%M:%S", tm_info);
    puts(buffer);

    return 0;
}
```
이 코드를 실행하면 현재 시간이 'YYYY-MM-DD HH:MM:SS' 형식의 문자열로 출력됩니다.

## 자세히 알아보기
역사적 관점에서 볼 때, 날짜를 문자열로 변환하는 것은 프로그래밍의 초기부터 필요로 하는 기능 중 하나였습니다. 이 기능을 통해 날짜 정보를 파일 이름에 포함시키거나, 로그 메시지를 작성하는 등의 작업이 가능해졌습니다.

C 언어에서는 기본적으로 `strftime` 함수를 사용해 날짜를 문자열로 변환할 수 있습니다. 그러나 `strftime`함수 외에도 사용자의 특정 요구사항을 충족시키기 위한 다양한 라이브러리가 존재합니다.

`strftime` 함수의 내부적인 구현을 살펴보면, 입력으로 받은 날짜를 지정한 형식의 문자열로 변환하는 작업이 일어납니다. 이 함수는 출력 문자열의 형식을 지정하는 것이 가능하므로 활용도가 높습니다.

## 관련 자료
- C 언어 표준 라이브러리에 관한 자세한 내용: [C Standard Library](https://en.wikipedia.org/wiki/C_standard_library)
- `strftime` 함수에 대한 더 깊은 이해를 위한 링크: [Example of strftime](http://www.cplusplus.com/reference/ctime/strftime/)
- 날짜와 시간에 관한 프로그래밍 팁: [C Programming Time-related tips](https://www.techonthenet.com/c_language/standard_library_functions/time_h/index.php)