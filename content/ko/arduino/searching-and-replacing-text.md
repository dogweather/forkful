---
title:    "Arduino: 텍스트 검색 및 대치하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜: 텍스트를 검색하고 대체하는 작업을 하는 이유
텍스트를 검색하고 대체하는 작업은 우리가 일상에서 흔히 사용하는 작업입니다. 컴퓨터에서 더 쉽고 빠르게 이 작업을 할 수 있도록 아두이노 프로그래밍을 배워보세요.

## 어떻게: 아두이노 프로그래밍에서 텍스트 검색과 대체하는 방법
아두이노에서 엔터 키를 누르면 특정 문자열을 검색하고 다른 문자열로 대체하는 프로그램을 작성해보겠습니다. 코드 블록은 Arduino 문법이 적용되어 있습니다.
```
Arduino.setup()
{
    String input = Serial.readString();
    // 입력된 문자열을 읽어옴
    String search = "apple";
    // 찾을 문자열
    String replace = "orange";
    // 대체할 문자열
    input.replace(search, replace);
    // 입력된 문자열에서 "apple"을 "orange"로 대체
    Serial.println(input);
    // 대체된 문자열을 시리얼 모니터로 출력
}
```

위와 같이 코드를 작성하고 업로드하면 시리얼 모니터에서 원하는 문자열을 검색하고 대체하는 작업을 할 수 있습니다. 예를 들어, 아두이노와 컴퓨터를 연결하고 시리얼 모니터에 "I love apple"이라는 문장을 입력하면 "I love orange"라는 문장이 출력됩니다.

## 깊이 파고들기: 텍스트 검색과 대체에 대해 더 알아보기
위 코드에서는 단순히 "apple"을 "orange"로 대체하도록 작성하였지만, 더욱 복잡한 코드를 작성하여 여러 개의 문자열을 한 번에 검색하고 대체할 수도 있습니다. 또한 특정 문자열 패턴을 이용해 대체할 수도 있습니다. 아두이노에서 사용 가능한 여러 가지 문자열 함수를 배우면 더 다양한 방법으로 텍스트 검색과 대체를 할 수 있습니다.

## 더 알아보기
- [아두이노 공식 사이트](https://www.arduino.cc/)
- [아두이노 텍스트 함수 공식 문서](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [아두이노 프로그래밍을 위한 한국어 강좌](http://www.hwain.net/study/arduino/index.php#p_lang)
- [아두이노 커뮤니티 사이트](https://playground.arduino.cc/)

# 참고 자료
https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet(Korean)