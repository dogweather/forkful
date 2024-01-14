---
title:                "Arduino: 텍스트 검색과 교체"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
아두이노 프로그래밍을 하다보면 종종 코드에서 특정 텍스트를 찾고 대체해야 할 때가 있습니다. 이를 위해 검색 및 대체 기능은 매우 유용합니다. 이를 통해 특정 텍스트를 쉽게 찾아 변경할 수 있으며, 코드를 작업하고 디버그하는 데 더 많은 시간을 할애할 수 있습니다.

## 방법
텍스트를 검색하고 대체하는 방법은 매우 간단합니다. 우선 `String` 라이브러리를 이용하여 검색하려는 텍스트를 지정합니다. 그리고 `replace()` 함수를 호출하여 대체할 텍스트를 지정합니다. 아래는 간단한 예제 코드입니다.

```Arduino
// 찾고 대체할 문자열 선언
String search_string = "안녕하세요";
String replace_string = "Hello";

// 문자열 검색 후 대체
String new_string = search_string.replace("안녕", "Hello");
```

위 코드를 실행하면, "안녕하세요"가 "Hello하세요"로 대체됩니다. 또한 대체하고자 하는 텍스트가 여러 개 있을 경우, 모두 한 번에 대체가 가능합니다.

```Arduino
// 찾고 대체할 문자열 선언
String search_string = "안녕하세요";
String replace_string = "Hello";

// 문자열 검색 후 대체
String new_string = search_string.replace("안녕", "Hello");
String final_string = new_string.replace("하세요", "there");

// 결과 출력
Serial.println(final_string);
// 출력: Hellothere
```

위 예제에서는 `replace()` 함수를 두 번 호출하여 찾고 대체한 텍스트를 더 세부적으로 지정할 수 있었습니다.

## 깊이 파헤치기
검색 및 대체 기능은 매우 간단하지만, 더 깊이 파헤쳐보면 더 많은 옵션을 활용할 수 있습니다. 예를 들어, `replace()` 함수는 대체되는 텍스트의 횟수를 지정할 수 있습니다.

```Arduino
// 찾고 대체할 문자열 선언
String search_string = "안녕하세요";
String replace_string = "Hello";

// 문자열 검색 후 대체 (대체 횟수 지정)
String new_string = search_string.replace("안녕", "Hello", 1);
```

위 코드에서는 대체 횟수를 1로 지정하였기 때문에, 첫 번째 "안녕"만 대체됩니다. 대체 횟수를 지정하지 않으면, 모든 "안녕"이 "Hello"로 대체됩니다.

또한 `replace()` 함수 외에도 `replaceAll()` 함수를 이용하여 모든 찾는 텍스트를 한 번에 대체할 수도 있습니다. 이 함수는 `replace()` 함수와 유사하지만, 검색하는 텍스트가 있을 경우 계속해서 대체가 이루어 집니다.

## 더 알아보기
위의 예제 코드에서 사용한 `String` 라이브러리 외에도 `StringReplace.h` 라이브러리를 이용하는 방법도 있습니다. 또한, 다른 유용한 함수들도 있으니 관련 자료를 참고해보시기 바랍니다.

## 참고자료
- [Arduino String 클래스 관련 문서](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [StringReplace.h 라이브러리 관련 자료](https://github.com/avishorp/StringReplace)