---
aliases:
- /ko/arduino/removing-quotes-from-a-string/
date: 2024-01-26 03:36:51.017491-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\
  \uD55C\uB2E4\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uB97C \uAC10\uC2F8\uACE0 \uC788\
  \uB294 \uBAA8\uB4E0 \uB2E8\uC77C(`'`) \uB610\uB294 \uC774\uC911(`\"`) \uB530\uC634\
  \uD45C \uBB38\uC790 \uC778\uC2A4\uD134\uC2A4\uB97C \uC81C\uAC70\uD558\uB294 \uAC83\
  \uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC885\uC885 \uC785\uB825\uC744 \uC815\uD654\uD558\uAC70\uB098, \uBB38\uC790\uC5F4\
  \uC744 \uBE44\uAD50\uD558\uAE30 \uC804\uC5D0 \uC900\uBE44\uD558\uAC70\uB098, \uBB38\
  \uC790\uC5F4 \uB0B4\uC6A9\uC758 \uC77C\uBD80\uB85C \uC2E4\uC218\uB85C \uB530\uC634\
  \uD45C\uAC00 \uD3EC\uD568\uB420 \uC218 \uC788\uB294\u2026"
lastmod: 2024-02-18 23:09:06.601315
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uB97C \uAC10\uC2F8\uACE0 \uC788\uB294\
  \ \uBAA8\uB4E0 \uB2E8\uC77C(`'`) \uB610\uB294 \uC774\uC911(`\"`) \uB530\uC634\uD45C\
  \ \uBB38\uC790 \uC778\uC2A4\uD134\uC2A4\uB97C \uC81C\uAC70\uD558\uB294 \uAC83\uC744\
  \ \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\
  \uC885 \uC785\uB825\uC744 \uC815\uD654\uD558\uAC70\uB098, \uBB38\uC790\uC5F4\uC744\
  \ \uBE44\uAD50\uD558\uAE30 \uC804\uC5D0 \uC900\uBE44\uD558\uAC70\uB098, \uBB38\uC790\
  \uC5F4 \uB0B4\uC6A9\uC758 \uC77C\uBD80\uB85C \uC2E4\uC218\uB85C \uB530\uC634\uD45C\
  \uAC00 \uD3EC\uD568\uB420 \uC218 \uC788\uB294\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
문자열에서 따옴표를 제거한다는 것은 텍스트를 감싸고 있는 모든 단일(`'`) 또는 이중(`"`) 따옴표 문자 인스턴스를 제거하는 것을 의미합니다. 프로그래머들은 종종 입력을 정화하거나, 문자열을 비교하기 전에 준비하거나, 문자열 내용의 일부로 실수로 따옴표가 포함될 수 있는 텍스트 데이터를 처리하기 위해 이 작업을 수행합니다.

## 방법:
Arduino에서 문자열에서 따옴표를 제거하려면, 문자들을 반복하여 따옴표 문자 없이 문자열을 재구성할 수 있습니다. 예를 들면:

```arduino
String removeQuotes(String str) {
  String result = ""; // 결과를 저장할 빈 문자열 생성
  for (int i = 0; i < str.length(); i++) {
    if (str[i] != '"' && str[i] != '\'') { // 각 문자 확인
      result += str[i]; // 따옴표가 아니면 결과에 추가
    }
  }
  return result;
}

void setup() {
  Serial.begin(9600);
  String testStr = "'Hello, World!'";
  Serial.println(removeQuotes(testStr)); // 출력해야 함: Hello, World!
}

void loop() {
  // 여기서 할 일 없음
}
```

시리얼 모니터에서의 샘플 출력은:
```
Hello, World!
```

## 심층 탐구
문자열에서 문자를 제거하는 개념은 Arduino에만 국한된 것이 아니며, 많은 프로그래밍 환경에서 흔히 볼 수 있습니다. 역사적으로, 문자열 조작 함수는 개발자들이 데이터를 효율적으로 정리하고 파싱할 수 있도록 프로그래밍 언어의 핵심 부분이었습니다.

위에서 보여진 것처럼 수동으로 반복하여 새 문자열을 만드는 방법 외에도 대체 방법들이 있습니다. 예를 들어, `replace()` 메소드를 사용하여 따옴표를 빈 문자열로 대체할 수 있지만, 가독성 관리와 이스케이프 문자를 다루는 것과 관련하여 타협점이 있습니다.

```arduino
String removeQuotes(String str) {
  str.replace("\"", ""); // 모든 이중 따옴표를 대체
  str.replace("\'", ""); // 모든 단일 따옴표를 대체
  return str;
}
```

타협점을 이해하는 것은 중요합니다. 반복 메소드는 긴 문자열에 대해 더 느릴 수 있지만 명시적이며 커스터마이즈하기 쉽습니다(예를 들어, 선행 및 후행 따옴표만 제거해야 하는 경우처럼). `replace()` 메소드는 더 간결하고 일반적으로 더 빠르지만, 문자열 내부에 이스케이프된 따옴표 문자를 처리해야 하는 경우 더 까다로워집니다.

## 참고
- Arduino 문자열 참조: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- W3Schools의 C++ 문자열 조작 가이드(Arduino의 언어와 관련됨): https://www.w3schools.com/cpp/cpp_strings.asp
- C++(Arduino의 기반 언어)에서 문자열 조작에 관한 Stack Overflow 토론: https://stackoverflow.com/questions/tagged/string+cpp
