---
title:                "문자열 대문자화"
aliases:
- ko/arduino/capitalizing-a-string.md
date:                  2024-02-03T19:05:09.341909-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열의 첫 글자를 대문자로 변환하는 것은 문자열의 각 단어의 첫 문자를 대문자로 변환하면서 나머지는 소문자로 유지하는 작업을 말합니다. 이 작업은 데이터 포맷팅 및 사용자 입력의 정규화에 흔히 사용되어 일관성을 유지하고 가독성을 향상시킵니다.

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
