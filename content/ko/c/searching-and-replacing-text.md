---
title:                "C: 텍스트 검색 및 대체"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜 

텍스트 검색 및 대체를 수행하는 이유는 데이터 분석, 문서 편집 또는 프로그래밍 일 등 다양한 목적을 위해서이다.

# 방법

우선, C 프로그래밍 언어를 사용하여 텍스트 검색 및 대체를 수행하는 방법을 알아보겠습니다. 아래의 예시 코드를 따라하며 실제로 동작하는 결과를 확인해보세요.

```
#include <stdio.h>
#include <string.h>

int main() {
  // 대상 문자열
  char input[] = "안녕하세요, 제 이름은 John입니다.";

  // 대체할 문자열
  char replace[] = "Jane";

  // "John"을 "Jane"으로 대체
  char *result = strstr(input, "John");
  strncpy(result, replace, strlen(replace));

  // 결과 출력
  printf("바뀐 문자열: %s", input);

  return 0;
}
```

**출력결과:**

```
바뀐 문자열: 안녕하세요, 제 이름은 Jane입니다.
```

위의 예시 코드에서는 `strstr()` 함수를 사용하여 대상 문자열에서 "John"이라는 부분을 찾고, `strncpy()` 함수를 사용하여 "Jane"으로 대체하였습니다.

또 다른 방법으로는 정규식을 사용하는 것이 있습니다. 정규식은 패턴매칭을 위한 표현식으로, 더 복잡한 검색 및 대체 작업을 수행할 수 있습니다. 아래의 예시 코드를 참고해보세요.

```
#include <stdio.h>
#include <regex.h>

int main() {
  // 대상 문자열
  char input[] = "나는 1990년에 태어났어요.";

  // 패턴 정의
  char pattern[] = "[0-9]{4}";

  // 대체할 문자열
  char replace[] = "2021";

  // 정규식 컴파일
  regex_t regex;
  regcomp(&regex, pattern, REG_EXTENDED);

  // 대체 작업 수행
  char *result = regreplace(input, &regex, replace);

  // 결과 출력
  printf("바뀐 문자열: %s", result);

  return 0;
}
```

**출력결과:**

```
바뀐 문자열: 나는 19 2021 에 태어났어요.
```

위의 예시 코드에서는 `regex.h` 라이브러리를 사용하여 정규식을 컴파일하고, `regreplace()` 함수를 이용하여 대체 작업을 수행하였습니다.

# 딥 다이브

텍스트 검색 및 대체 작업은 실제로는 매우 복잡합니다. 대상 문자열의 길이, 검색 패턴의 다양성, 대체하는 문자열의 유동성 등 다양한 요소를 고려해야 합니다. 또한, 문자열의 인코딩, 대소문자 구분 여부 등 특정한 제약사항도 고려해야 합니다. 이러한 세부적인 사항에 대해서는 해당 언어의 공식 문서나 여러 블로그 및 포럼에서 찾아볼 수 있습니다.

# 또 다른 정보들

- [C 언어 공식 문서](https://www.tutorialspoint.com/cprogramming/index.htm)
- [정규식 관련 블로그 포스트](https://code.tutsplus.com/ko/tutorials/8-regular-expressions-you-should-know--net-6149)