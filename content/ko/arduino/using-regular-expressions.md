---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"

category:             "Arduino"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
정규 표현식은 문자열에서 패턴을 찾기 위한 강력한 도구입니다. 프로그래머들은 정규 표현식을 사용하여 데이터 검증, 검색 및 문자열 조작을 빠르고 효율적으로 수행합니다.

## How to: (어떻게 하나요?)
Arduino에는 기본적으로 정규 표현식 라이브러리가 내장되어 있지 않아서, 외부 라이브러리를 설치해야 합니다. 예를 들어, ‘Regexp’ 라이브러리를 사용할 수 있습니다.

```Arduino
#include <Regexp.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) { ; // wait for serial port to connect.

  MatchState ms;
  char result;
  
  ms.Target ("The rain in Spain");
  
  result = ms.Match ("(S|s)ain");
  
  if (result == REGEXP_MATCHED) {
    Serial.println("Match!");
  } else {
    Serial.println("No Match!");
  }
}

void loop() {
  // put your main code here, to run repeatedly:
}
```
샘플 출력:
```
Match!
```

## Deep Dive (심층 탐구)
정규 표현식은 1950년대에 만들어졌으며, 문자열 처리에 혁명을 가져왔습니다. Arduino에서는 완전한 정규 표현식 지원을 기대할 수 없지만, 'Regexp'와 같은 라이브러리를 사용하면 기본적인 패턴 매칭이 가능합니다. 메모리 제약으로 인해, 대규모 문자열 처리나 복잡한 표현식을 피해야 합니다.

## See Also (참고 자료)
- Arduino Regexp library on GitHub: [https://github.com/nickgammon/Regexp](https://github.com/nickgammon/Regexp)
- Regular expression syntax reference: [https://www.regular-expressions.info/reference.html](https://www.regular-expressions.info/reference.html)
- Arduino 공식 포럼, 정규 표현식 사용법 토론: [http://forum.arduino.cc/](http://forum.arduino.cc/)
