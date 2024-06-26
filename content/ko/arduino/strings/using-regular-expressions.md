---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:22.379827-07:00
description: "\uBC29\uBC95: \uC544\uB450\uC774\uB178\uB294 \uD45C\uC900 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uC5D0\uC11C \uC9C1\uC811\uC801\uC73C\uB85C \uC815\uADDC \uD45C\
  \uD604\uC2DD\uC744 \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4. \uD558\uC9C0\
  \uB9CC, \uAE30\uBCF8 \uBB38\uC790\uC5F4 \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ \uAC04\uB2E8\uD55C \uD328\uD134\uC5D0 \uB300\uD574 \uC815\uADDC \uD45C\uD604\uC2DD\
  \uACFC \uC720\uC0AC\uD55C \uAE30\uB2A5\uC744 \uAD6C\uD604\uD558\uAC70\uB098, \uBCF4\
  \uB2E4 \uBCF5\uC7A1\uD55C \uC694\uAD6C\uC0AC\uD56D\uC758 \uACBD\uC6B0 `regex`\uC640\
  \ \uAC19\uC740 \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD1B5\uD569\uD558\
  \uC5EC \uC0AC\uC6A9\uD560 \uC218\u2026"
lastmod: '2024-03-13T22:44:55.593795-06:00'
model: gpt-4-0125-preview
summary: "\uC544\uB450\uC774\uB178\uB294 \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uC5D0\uC11C \uC9C1\uC811\uC801\uC73C\uB85C \uC815\uADDC \uD45C\uD604\uC2DD\uC744\
  \ \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uC2B5\uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

## 방법:
아두이노는 표준 라이브러리에서 직접적으로 정규 표현식을 지원하지 않습니다. 하지만, 기본 문자열 함수를 사용하여 간단한 패턴에 대해 정규 표현식과 유사한 기능을 구현하거나, 보다 복잡한 요구사항의 경우 `regex`와 같은 제3자 라이브러리를 통합하여 사용할 수 있습니다.

### 정규 표현식 없이 기본 문자열 매칭
하위 문자열 찾기와 같은 기본적인 요구사항을 위해서는 `String.indexOf()` 함수를 사용할 수 있습니다:
```cpp
String data = "Sensor value: 12345";
int index = data.indexOf("value:");
if (index != -1) {
  String value = data.substring(index + 6).trim();
  Serial.println(value); // 출력: 12345
}
```

### 정규 표현식을 위한 제3자 라이브러리 사용하기
보다 복잡한 패턴을 다루기 위해서는 `regex`와 같은 라이브러리를 고려할 수 있습니다. 라이브러리를 설치한 후, 다음과 같이 사용할 수 있습니다:

1. **설치**: `regex` 라이브러리는 아두이노 라이브러리 관리자에서 바로 사용할 수 없을 수 있으므로, 신뢰할 수 있는 소스에서 다운로드 받아 아두이노 라이브러리 폴더에 수동으로 추가해야 할 수 있습니다.

2. **사용 예시**:
라이브러리가 표준 정규 표현식 구현과 유사한 기능을 제공한다고 가정하면, 다음과 같이 사용할 수 있습니다:

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // 시리얼이 준비될 때까지 대기
  
  regex_t reg;
  const char* pattern = "[0-9]+"; // 숫자 시퀀스와 일치
  regcomp(&reg, pattern, REG_EXTENDED);
  
  const char* test_str = "Sensor value: 12345";
  
  regmatch_t matches[1];
  if (regexec(&reg, test_str, 1, matches, 0) == 0) {
    // 일치하는 부분을 추출 및 출력
    int start = matches[0].rm_so;
    int end = matches[0].rm_eo;
    char match[end-start+1];
    strncpy(match, test_str + start, end-start);
    match[end-start] = '\0';
    
    Serial.print("일치하는 항목 찾음: ");
    Serial.println(match); // 출력: 12345
  } else {
    Serial.println("일치하는 항목 없음");
  }
  
  regfree(&reg); // 정규 표현식에 할당된 메모리 해제
}

void loop() {
  // 반복적으로 실행될 주요 코드를 이곳에 작성:
}
```

**참고**: 여기에서 사용된 구문 및 특정 함수는 설명을 위한 것이며, 선택한 `regex` 라이브러리의 실제 구현 세부사항에 따라 다를 수 있습니다. 정확하고 최신 정보는 항상 라이브러리의 문서를 참조하세요.
