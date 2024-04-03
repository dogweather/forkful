---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:09.341909-07:00
description: "\uBC29\uBC95: \uD558\uB4DC\uC6E8\uC5B4\uC640 \uC0C1\uD638\uC791\uC6A9\
  \uD558\uB294 \uAC83\uC73C\uB85C \uC8FC\uB85C \uC54C\uB824\uC9C4 \uC544\uB450\uC774\
  \uB178\uC5D0\uB294 `String` \uAC1D\uCCB4\uB97C \uD1B5\uD55C \uAE30\uBCF8\uC801\uC778\
  \ \uBB38\uC790\uC5F4 \uC870\uC791 \uAE30\uB2A5\uB3C4 \uD3EC\uD568\uB418\uC5B4 \uC788\
  \uC2B5\uB2C8\uB2E4. \uD558\uC9C0\uB9CC, \uACE0\uAE09 \uC5B8\uC5B4\uC5D0\uC11C \uBCFC\
  \ \uC218 \uC788\uB294 \uC9C1\uC811\uC801\uC778 `capitalize` \uD568\uC218\uB294 \uC5C6\
  \uC2B5\uB2C8\uB2E4. \uB530\uB77C\uC11C, \uBB38\uC790\uC5F4\uC744 \uC21C\uD68C\uD558\
  \uBA70 \uB300\uC18C\uBB38\uC790 \uBCC0\uD658\uC744\u2026"
lastmod: '2024-03-13T22:44:55.584037-06:00'
model: gpt-4-0125-preview
summary: "\uD558\uB4DC\uC6E8\uC5B4\uC640 \uC0C1\uD638\uC791\uC6A9\uD558\uB294 \uAC83\
  \uC73C\uB85C \uC8FC\uB85C \uC54C\uB824\uC9C4 \uC544\uB450\uC774\uB178\uC5D0\uB294\
  \ `String` \uAC1D\uCCB4\uB97C \uD1B5\uD55C \uAE30\uBCF8\uC801\uC778 \uBB38\uC790\
  \uC5F4 \uC870\uC791 \uAE30\uB2A5\uB3C4 \uD3EC\uD568\uB418\uC5B4 \uC788\uC2B5\uB2C8\
  \uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 방법:
하드웨어와 상호작용하는 것으로 주로 알려진 아두이노에는 `String` 객체를 통한 기본적인 문자열 조작 기능도 포함되어 있습니다. 하지만, 고급 언어에서 볼 수 있는 직접적인 `capitalize` 함수는 없습니다. 따라서, 문자열을 순회하며 대소문자 변환을 적용함으로써 대문자화를 구현합니다.

여기 제3자 라이브러리를 사용하지 않는 기본 예제가 있습니다:

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // 입력이 비어있으면 빈 문자열 반환
  }
  input.toLowerCase(); // 먼저 전체 문자열을 소문자로 변환
  input.setCharAt(0, input.charAt(0) - 32); // 첫 글자를 대문자로 변환
  
  // 공백 뒤에 오는 글자를 대문자로 변환
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // 출력: "Hello Arduino World"
}

void loop() {
  // 빈 루프
}
```

이 코드 스니펫은 `capitalizeString` 함수를 정의하여 전체 문자열을 소문자로 변환하여 케이스를 표준화합니다. 그런 다음 첫 글자와 공백 뒤에 오는 글자를 대문자로 변환하여 입력 문자열의 각 단어를 효과적으로 대문자화합니다. 이 기본 구현은 ASCII 문자 인코딩을 가정하고 있으며 전체 유니코드 지원을 위한 조정이 필요할 수 있습니다.

현재, 아두이노 생태계에서 문자열 조작을 위한 널리 채택된 제3자 라이브러리는 주로 하드웨어 상호작용과 효율성에 중점을 둔 것이기 때문에 거의 없습니다. 하지만, 제공된 예제는 아두이노 프로그래밍 환경 내에서 문자열 대문자화를 달성하는 간단한 방법입니다.
