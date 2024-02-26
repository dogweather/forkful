---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:22.379827-07:00
description: "\uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC740 \uC8FC\uB85C \uBB38\uC790\
  \uC5F4\uC758 \uC77C\uCE58 \uBC0F \uC870\uC791\uC744 \uC704\uD574 \uC0AC\uC6A9\uB418\
  \uB294 \uAC80\uC0C9 \uD328\uD134\uC744 \uC815\uC758\uD558\uB294 \uBB38\uC790\uC758\
  \ \uC2DC\uD000\uC2A4\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC544\uB450\uC774\uB178 \uD504\uB85C\uC81D\uD2B8\uC5D0\uC11C \uC9C1\uB82C \uC785\
  \uB825\uC744 \uD30C\uC2F1\uD558\uAC70\uB098, \uC0AC\uC6A9\uC790 \uC785\uB825\uC744\
  \ \uAC80\uC99D\uD558\uAC70\uB098, \uBB38\uC790\uC5F4\uC5D0\uC11C \uB370\uC774\uD130\
  \uB97C \uCD94\uCD9C\uD558\uB294 \uB4F1\uC758 \uC791\uC5C5\uC744 \uC704\uD574 \uC815\
  \uADDC \uD45C\uD604\uC2DD\uC744 \uD65C\uC6A9\uD558\uC5EC\u2026"
lastmod: '2024-02-25T18:49:52.582937-07:00'
model: gpt-4-0125-preview
summary: "\uC815\uADDC \uD45C\uD604\uC2DD(regex)\uC740 \uC8FC\uB85C \uBB38\uC790\uC5F4\
  \uC758 \uC77C\uCE58 \uBC0F \uC870\uC791\uC744 \uC704\uD574 \uC0AC\uC6A9\uB418\uB294\
  \ \uAC80\uC0C9 \uD328\uD134\uC744 \uC815\uC758\uD558\uB294 \uBB38\uC790\uC758 \uC2DC\
  \uD000\uC2A4\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC544\
  \uB450\uC774\uB178 \uD504\uB85C\uC81D\uD2B8\uC5D0\uC11C \uC9C1\uB82C \uC785\uB825\
  \uC744 \uD30C\uC2F1\uD558\uAC70\uB098, \uC0AC\uC6A9\uC790 \uC785\uB825\uC744 \uAC80\
  \uC99D\uD558\uAC70\uB098, \uBB38\uC790\uC5F4\uC5D0\uC11C \uB370\uC774\uD130\uB97C\
  \ \uCD94\uCD9C\uD558\uB294 \uB4F1\uC758 \uC791\uC5C5\uC744 \uC704\uD574 \uC815\uADDC\
  \ \uD45C\uD604\uC2DD\uC744 \uD65C\uC6A9\uD558\uC5EC\u2026"
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
정규 표현식(regex)은 주로 문자열의 일치 및 조작을 위해 사용되는 검색 패턴을 정의하는 문자의 시퀀스입니다. 프로그래머들은 아두이노 프로젝트에서 직렬 입력을 파싱하거나, 사용자 입력을 검증하거나, 문자열에서 데이터를 추출하는 등의 작업을 위해 정규 표현식을 활용하여 데이터 처리의 효율성과 유연성을 향상시킵니다.

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
