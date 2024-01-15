---
title:                "패턴과 일치하는 문자를 삭제하는 방법"
html_title:           "Arduino: 패턴과 일치하는 문자를 삭제하는 방법"
simple_title:         "패턴과 일치하는 문자를 삭제하는 방법"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 특정 패턴과 일치하는 문자를 삭제하는 프로그램을 만들어야 하는 이유는 무엇일까요?

String 형식의 데이터를 다루는 일은 프로그래밍에서 매우 흔한 작업입니다. 이때 문자열 안에는 원하는 정보가 포함되어 있을 수도 있지만, 필요 없는 정보도 함께 포함되어 있을 수 있습니다. 이럴 때 필요한 것이 바로 문자열에서 특정 패턴과 일치하는 문자를 삭제하는 것입니다. 예를 들어, 특정 단어를 찾아서 삭제하면서 원하는 정보만 남게 만들거나, 특정 문자를 일괄적으로 삭제해서 문자열을 깔끔하게 정리하는 등의 작업에 활용할 수 있습니다.

## 사용 방법

이제 Arduino를 사용하여 문자열에서 특정 패턴과 일치하는 문자를 삭제하는 방법을 알아보겠습니다. 아래 코드를 참고하여 실제로 실행해보세요.

```Arduino
String input = "Hello, world!";
String pattern = "l";

for (int i = 0; i < input.length(); i++) {
  if (input.substring(i, i+1) == pattern) {
    input.remove(i, 1);
  }
}

Serial.println(input);
```

이 코드는 "Hello, world!"라는 문자열에서 "l"이라는 문자를 일괄적으로 삭제하고, 결과인 "Heo, word!"를 출력합니다. 이 코드를 보면 문자열을 순서대로 검사하면서 일치하는 문자를 발견하면 `input.remove()` 함수를 사용해서 해당 문자를 삭제하고, 이후의 문자들을 앞으로 한 자리씩 앞당기는 방식을 사용하고 있습니다.

혹은, 더 간단한 방법으로는 `replace()` 함수를 사용하는 것입니다. 아래 코드를 참고해주세요.

```Arduino
String input = "Hello, world!";
String pattern = "l";

input.replace(pattern, "");

Serial.println(input);
```

`replace()` 함수는 첫 번째 매개변수로 전달한 값을 두 번째 매개변수로 전달한 값으로 바꾸는 역할을 합니다. 위의 코드에서는 "l"을 빈 문자열로 바꿔서 삭제하는 효과를 낼 수 있습니다.

## 디테일

더 깊이 들어가서 이제 문자열에서 특정 패턴에 해당하는 문자를 삭제하는 방법에 대해 더 자세히 알아보겠습니다.

먼저, `substring()` 함수를 사용해서 문자열을 자를 수 있습니다. 이 함수는 첫 번째 매개변수로 시작 인덱스를, 두 번째 매개변수로 종료 인덱스를 전달받습니다. 만약 두 번째 매개변수를 생략하면 시작 인덱스부터 문자열의 끝까지 추출합니다. 이 함수를 사용하면 문자열 중간에서 원하는 부분을 남기고 나머지 부분을 삭제할 수 있습니다. 또는 `charAt()` 함수를 사용해서 특정 인덱스의 문자를 추출할 수도 있습니다.

또한, `indexOf()` 함수를 사용하면 문자열에서 특정 패턴이 처음 등장하는 인덱스를 알아낼 수 있습니다. 이 함수는 첫 번째 매개변수로 검색할 패턴을, 두 번째 매개변수로 시작 인덱스를 전달받습니다. 이 함수를 사용하면 첫 번째 등장하는 패턴의 인덱스를 알 수 있고, 그 인덱스를 기준으로 문자열을 자르는 등의 작업을 할 수 있습니다.

이외