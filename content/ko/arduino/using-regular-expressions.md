---
title:                "정규 표현식 사용하기"
html_title:           "Bash: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

정규 표현식은 패턴 매칭 및 검색에 사용되는 특수 식입니다. 프로그래머들은 이를 사용하여 코드 내 문자열을 효율적으로 핸들링 할 수 있습니다.

## 사용 방법:

다음은 아두이노에서 정규 표현식을 사용하는 방법에 대한 코드 예제입니다.

```Arduino
#include <regex.h>

void setup() {
  char input[] = "Arduino123";
  regex_t regex;
  int ret;
  ret = regcomp(&regex, "[0-9]", 0);
  ret = regexec(&regex, input, 0, NULL, 0);

  if (!ret) {
    Serial.println("Match found!");
  } else if (ret==REG_NOMATCH) {
    Serial.println("No match!");
  }
}

void loop() {
  // put your main code here, to run repeatedly:
}
```

이 코드 조각은 "Arduino123"에서 숫자를 검색하고 일치 항목을 찾지 못하면 "No match!"를 출력합니다.

## 심화 학습:

1. **역사적인 맥락**: 정규 표현식(regular expression)은 1950년대에 개발되어 패턴 매칭용 알고리즘이라는 주요 개념을 소개했습니다.

2. **대안**: 정규 표현식 외에도 `strstr()`, `strcmp()` 등과 같은 문자열 비교 함수를 사용할 수 있지만, 정규 표현식은 그들보다 더 가미된 예기치 못한 패턴을 쉽게 잡아냅니다.

3. **구현 세부 정보**: 아두이노는 펌웨어 수준에서 정규 표현식 지원이 없습니다. 하지만, 정규 표현식 라이브러리를 사용하여 이 기능을 추가할 수 있습니다.

## 참고 자료:

다음은 이 주제와 관련된 추가 리소스에 대한 링크입니다.

- [Arduino Language Reference](https://www.arduino.cc/reference/en/)
- [Arduino Libraries](https://www.arduino.cc/en/guide/libraries)

이 문서가 아두이노에서 정규 표현식 사용의 기본을 이해하는데 도움이되기를 바랍니다.