---
title:                "Arduino: 정규 표현식 사용하기."
simple_title:         "정규 표현식 사용하기."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

왜 우리는 정규 표현식을 사용할까요? 정규 표현식은 사용자가 입력한 캐릭터 패턴을 매칭하기 위해 사용됩니다. 이는 입력 데이터가 원하는 패턴에 맞는지를 검증하는 데 사용될 수 있습니다. 예를 들어, 서식을 철자와 특수문자로 구성된 이메일 주소의 경우 정규 표현식을 사용하면 이메일 주소가 유효한 형식인지를 쉽게 확인할 수 있습니다.

## 사용 방법

정규 표현식을 사용하는 가장 간단한 방법은 `matches()` 함수를 사용하는 것입니다. 아두이노에서는 `matches()` 함수를 사용하여 입력된 문자열이 정규 표현식과 일치하는지를 확인할 수 있습니다. 해당 함수는 결과값으로 `true` 또는 `false`를 반환합니다. 아래 예제 코드를 참고해보세요.

```Arduino
String input = "25 로봇";
// 로봇이라는 단어를 포함하고 있는지 확인
if (input.matches(".*로봇.*")) {
  Serial.println("이 문자열은 로봇을 포함합니다.");
}
```

위 예제 코드에서는 `"25 로봇"`이라는 문자열이 정규 표현식 `.*로봇.*`와 일치하기 때문에 `Serial.println` 함수가 실행될 것입니다.

## 자세히 알아보기

정규 표현식을 사용하여 좀 더 복잡한 문자열 처리를 할 수 있습니다. 영어로 된 이메일 주소를 입력 받아 한글 도메인을 가지고 있는지를 판단하는 예제를 살펴보겠습니다.

```Arduino
String input = "example@아두이노.한국";
// 한글 도메인을 포함하고 있는지 확인
if (input.matches(".*@(([ㄱ-ㅎ가-힣])+)[.](([ㄱ-ㅎ가-힣])+)$")) {
  Serial.println("이메일 주소에 한글 도메인이 포함되어 있습니다.");
}
```

위 예제에서는 `@` 기준으로 분리된 이메일 주소의 도메인 부분을 정규 표현식을 사용하여 한글로 구성되어 있는지를 판단하고 있습니다. `"아두이노.한국"`이라는 도메인이 한글로 구성되어 있기 때문에 `Serial.println` 함수가 실행될 것입니다.

## 관련 정보

위에서 살펴본 예제 외에도 다양한 방법으로 정규 표현식을 활용할 수 있습니다. 아래 링크를 통해 더 많은 정보를 얻어보세요.

- [아두이노 공식 문서](https://www.arduino.cc/reference/en/language/functions/communication/matches/)
- [정규 표현식 기초](https://regexone.com/)
- [정규 표현식 게임](https://regexcrossword.com/)