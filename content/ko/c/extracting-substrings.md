---
title:                "C: 문자열 추출"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜?

왜 문자열에서 부분 문자열을 추출하는 방법을 배워야 할까요? 부분 문자열을 추출하는 것은 문자열 처리에 있어서 필수적인 기술입니다. 예를 들어, 사용자로부터 입력받은 문자열에서 원하는 정보만을 추출하여 데이터베이스에 저장하거나, 특정한 문자열 패턴을 찾기 위해 필요한 기술입니다.

## 방법

```C
// 문자열에서 부분 문자열 추출하기
#include <stdio.h>
#include <string.h>

int main() {
  char str[100]; // 사용자로부터 입력받을 문자열 변수
  int start, end; // 추출할 부분 문자열의 시작과 끝 인덱스
  char sub[100]; // 추출한 부분 문자열을 저장할 변수

  printf("문자열을 입력하세요: ");
  scanf("%s", str); // 사용자로부터 문자열 입력받기

  printf("추출할 부분 문자열의 시작과 끝 인덱스를 입력하세요: ");
  scanf("%d %d", &start, &end); // 추출할 부분 문자열의 시작과 끝 인덱스 입력받기

  // 문자열 함수인 strncpy를 사용하여 부분 문자열 추출
  strncpy(sub, str+start, end-start);
  sub[end-start] = '\0'; // 추출한 부분 문자열 마지막에 널 문자 추가

  printf("추출된 부분 문자열: %s\n", sub); // 추출한 부분 문자열 출력

  return 0;
}
```

출력 예시:

```bash
문자열을 입력하세요: Hello World
추출할 부분 문자열의 시작과 끝 인덱스를 입력하세요: 1 4
추출된 부분 문자열: ello
```

## 깊게 파고들기

부분 문자열을 추출하는 것은 문자열을 다루는데 중요한 기술입니다. 그렇기 때문에 C 프로그래밍에서 문자열 처리를 위해 자주 사용되는 문자열 함수에 대해 알아보는 것이 좋습니다. 예를 들어, `strncpy` 함수는 문자열에서 일부분을 추출해주는 함수 중 하나이며, 이외에도 다양한 함수들이 있으니 필요할 때마다 찾아서 사용하시면 됩니다.

## 참고자료

* [C 프로그래밍 - 문자열 관련 함수](https://modoocode.com/17)
* [C언어에서 문자열 추출하는 방법](https://ered.tistory.com/30)
* [C 문자열 처리 함수 모음](https://conceptdev.tk/ccc/232)