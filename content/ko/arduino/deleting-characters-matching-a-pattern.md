---
title:                "Arduino: 패턴과 일치하는 문자 삭제하기"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜 지우는가

여러분은 아두이노 프로젝트를 진행하고 있을 때, 문자열을 다루게 될 수도 있습니다. 이 때, 특정 패턴과 일치하는 문자를 삭제해야 할 때가 있을 수 있습니다. 이러한 작업은 데이터 처리 속도를 높이고 코드의 이해도를 높이는 데에 도움이 될 수 있습니다.

# 코딩으로 배우는 방법

아래의 예제 코드를 활용하여 특정 패턴과 일치하는 문자를 삭제하는 방법에 대해 알아보겠습니다.

```Arduino
String message = "어서 오세요! 안녕하세요?";
String pattern = "어서 오세요!";

message.replace(pattern, "");

Serial.println(message); // " 안녕하세요?"
```

### 결과

아두이노 시리얼 모니터에서 " 안녕하세요?"라는 결과를 확인할 수 있습니다.

# 깊이 들어가기

문자열을 다루는 데에는 여러 가지 방법이 있습니다. 위의 예제에서는 `replace()` 함수를 사용했지만, `substring()` 함수를 활용하거나 반복문을 사용해 문자열을 순회하며 삭제하는 등의 방법도 있습니다.

또한, 정규표현식을 사용하면 더욱 더 간단하게 특정 패턴과 일치하는 문자를 삭제할 수 있습니다. 정규표현식에 대해서는 다른 기회에 다뤄보겠습니다.

# See Also

- [Arduino 문자열 함수 사용법 (블로그 글)](https://jang.pro/arduino-reference-string-functions/)
- [Arduino String reference (공식 문서)](https://www.arduino.cc/reference/ko/language/variables/data-types/stringobject/)
- [정규표현식 (위키백과 문서)](https://ko.wikipedia.org/wiki/%EC%A0%95%EA%B7%9C%ED%91%9C%EC%A4%80%EC%8B%9D)