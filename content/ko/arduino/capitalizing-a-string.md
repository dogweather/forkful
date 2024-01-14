---
title:    "Arduino: 문자열 대문자로 바꾸기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜 : 문자열을 대문자로 변경하는 것을 하는 이유

문자열을 대문자로 변경하는 것은 프로그래밍에서 자주 사용되는 기능 중 하나입니다. 이를 이용해 사용자로부터 입력 받은 데이터를 정리하거나, 출력할 때 보다 더 명확하게 표시할 수 있습니다. 또한 대문자로 변경하는 것은 문자열을 공백 또는 다른 특수 문자로 구분할 때도 유용합니다.

## 하는 방법 : 아두이노에서 문자열을 대문자로 변경하는 방법

아두이노에서 문자열을 대문자로 변경하는 방법은 간단합니다. 먼저, 문자열을 담을 변수를 선언하고, 아래 코드와 같이 `toUpperCase()` 함수를 사용하여 대문자로 변경하면됩니다.

```
// 문자열 변수 선언
String str = "hello world";

// 대문자로 변경된 문자열 출력
Serial.println(str.toUpperCase());
```

위의 코드를 아두이노 보드에 업로드하고 시리얼 모니터를 열어보면 `HELLO WORLD`라는 대문자로 변경된 문자열이 출력될 것입니다.

## 깊게 들어가보자 : 문자열을 대문자로 변경하는 원리

문자열을 대문자로 변경하는 원리는 간단합니다. 아래 코드는 `toUpperCase()` 함수의 내부 코드 일부분입니다.

```
String String::toUpperCase() {
  String tmp;
  for(int i = 0; i < len; ++i) {
    tmp.concat(char(tolower(data[i].toInt())));
  }
  return tmp;
}
```

`toUpperCase()` 함수는 문자열을 `tmp` 이라는 임시 변수에 복사하고, `for` 루프를 사용하여 각 문자를 순서대로 확인합니다. `tolower()` 함수를 사용하여 각 문자를 소문자로 변경하고, 이를 `tmp` 변수에 `concat()` 함수를 사용하여 이어붙입니다. 마지막으로 `tmp` 변수를 `return`하여 대문자로 변경된 문자열을 반환합니다.

## 더 알아보기 : 문자열 함수 참고사이트

- [Arduino 공식 홈페이지 - `String` 객체](https://www.arduino.cc/reference/ko/language/variables/data-types/stringobject/)
- [arduino cookbook - 문자열 병합하기](https://www.arduino.cc/en/Tutorial/StringAdditionOperator)
- [arduino cookbook - 문자열 변경하기](https://www.arduino.cc/en/Tutorial/ChangingCase)

## 이어서 보기

- [문자열 처리에 대한 자세한 설명](https://www.arduino.cc/reference/ko/language/variables/data-types/string/functions/tostring/)
- [아두이노 커뮤니티 포럼 - 문자열 관련 질문과 답변](https://forum.arduino.cc/index.php?board=1.0)