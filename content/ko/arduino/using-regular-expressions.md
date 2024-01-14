---
title:                "Arduino: 정규 표현식 사용하기"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜
왜 누군가 정규 표현식을 사용하는 것에 참여할까요? 정규 표현식은 문자열에서 특정 패턴을 찾고 추출할 수 있는 강력한 도구입니다. 따라서 Arduino 프로그래밍에서도 매우 유용하게 사용될 수 있습니다.

## 방법
Arduino에서 정규 표현식을 사용하려면 `Regex` 라이브러리를 설치해야 합니다. 이 라이브러리는 `re` 라이브러리와 비슷한 함수를 제공하여 문자열에서 패턴을 찾고 추출할 수 있도록 합니다.

```arduino
#include <Regex.h>

String pattern = "hello (.*), my name is (.*)";
String message = "Hello John, my name is Jane";

Regex regex(pattern);
MatchState match;

regex.match(message, match);

// 매칭된 결과를 출력합니다.
Serial.println(match.getMatch(0)); // "hello John, my name is Jane"
Serial.println(match.getMatch(1)); // "John"
Serial.println(match.getMatch(2)); // "Jane"
```

위 코드에서는 `hello (.*), my name is (.*)`라는 패턴을 정의하고, `hello John, my name is Jane`라는 문자열에서 해당 패턴을 찾아 매칭된 결과를 출력하는 예제를 보여줍니다.

## 깊이 더 들어가기
정규 표현식은 아주 유연하게 사용할 수 있습니다. 예를 들어, `.*`는 어떤 문자열이든지 매칭시키는 와일드카드 연산자입니다. 이 와일드카드를 사용하여 문자열 내에서 패턴을 추출하거나 치환하는 작업을 할 수 있습니다. 또한 `()`를 사용하여 해당 부분을 그룹으로 묶어 추출할 수도 있습니다.

정규 표현식에서 더 복잡한 패턴을 사용하려면 메타 문자(meta characters)를 사용해야 합니다. 예를 들어, `.`는 아무 문자를 매칭시키는 와일드카드이지만 `\.`과 같이 백슬래시를 붙여주면 실제 점을 매칭시키게 됩니다.

더 자세한 내용은 Arduino의 공식 문서에서 `Regex` 라이브러리에 대해 알아보세요.

## 관련 링크
- [Regex 라이브러리 문서](https://www.arduino.cc/reference/en/libraries/regex/)
- [정규 표현식 기초](https://regexone.com/)
- [정규 표현식 연습 사이트](https://regexr.com/)
- [정규 표현식 빠르게 배우기](https://www.codecademy.com/learn/learn-regular-expressions)