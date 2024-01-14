---
title:    "Arduino: 문자열의 길이 찾기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# 왜

문자열의 길이를 찾는 것은 아두이노 프로그래밍에서 중요한 부분입니다. 문자열은 많은 프로젝트에서 사용되는 기본적인 데이터 형식이기 때문에 문자열의 길이를 찾을 수 있는 능력은 중요합니다. 문자열의 길이를 찾는 방법을 알기 위해 이 포스트를 읽으시기 바랍니다.

# 어떻게

문자열의 길이를 찾는 방법은 간단합니다. 아두이노 내장 함수인 `strlen()`을 사용하면 됩니다. 이 함수는 문자열의 길이를 리턴해주는데, 사용 방법은 다음과 같습니다.

```Arduino
// 문자열 선언
String str = "안녕하세요!";

// 문자열 길이 출력
Serial.println(strlen(str));
```

위 코드를 실행하면 "`8`"이 출력될 것입니다. 이는 "안녕하세요!"라는 문자열의 길이가 8이기 때문입니다.

# 딥 다이브

아마도 이런 궁금증이 생길 것입니다. "만약 변수에 숫자 뿐만 아니라 문자열도 섞인 경우에는 어떻게 하나요?" 이럴 때는 `strlen()` 함수를 사용할 수 없습니다. 대신 아두이노 내장 함수인 `String` 클래스의 `length()` 함수를 사용해야 합니다. 이 함수는 문자열의 길이뿐만 아니라 변수에 저장된 숫자 및 문자열의 총 길이를 리턴해줍니다. 예를 들어서 `"안녕하세요!"`라는 문자열과 숫자 `123`을 더해서 `String`으로 변수를 만들면 `length()` 함수를 이용하면 총 길이가 `12`가 될 것입니다.

```Arduino
// 문자열과 숫자를 더해서 String 변수 생성
String myString = "안녕하세요!" + String(123);

// 변수의 총 길이 출력
Serial.println(myString.length());
```

하지만 중요한 것은 `length()` 함수를 사용하려면 문자열 변수를 생성해야 한다는 것입니다. 그렇지 않으면 에러가 발생합니다.

# 참고

이 포스트에서는 아두이노에서 문자열 길이를 찾는 간단한 방법을 배웠습니다. 다른 방법으로는 `String` 클래스의 `length()` 함수를 사용하는 것도 있지만, 이 경우에는 변수 생성에 대해 유의하셔야 합니다.

참조 링크:
- [아두이노 공식 문서 - 문자열 다루기](https://www.arduino.cc/reference/ko/language/variables/data-types/stringobject/length/)
- [아두이노 공식 문서 - 문자열 관련 내장 함수](https://www.arduino.cc/reference/ko/language/variables/data-types/string/functions/)
- [String 클래스(Malpartida)](https://github.com/Malpartida/String)