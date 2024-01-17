---
title:                "문자열 보간하기"
html_title:           "Arduino: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

# What & Why?
문자열 보간이란 무엇인가요? 그리고 왜 프로그래머들은 이를 하는 것일까요?

문자열 보간은 특정 문자열 내에 변수나 값을 삽입하는 것을 의미합니다. 예를 들어, "Hello, my name is ___"라는 문자열에서 ___ 자리에 사용자의 이름을 삽입하는 것이 문자열 보간의 한 예시입니다.

프로그래머들은 문자열 보간을 사용하는 이유는 코드를 더 간결하고 읽기 쉽게 만들 수 있기 때문입니다. 코드 내에서 변수나 값을 중복해서 사용하지 않고 문자열 보간을 통해 한 번에 삽입할 수 있기 때문에 코드의 길이를 줄일 수 있고 유지 보수성도 좋아집니다.

# How to:
아두이노에서 문자열 보간을 어떻게 사용할 수 있을까요?

기본적인 문자열 보간 방법은 크게 두 가지이며 모두 아두이노 내장 함수를 사용합니다.

첫 번째 방법은 `String` 클래스의 `format()` 함수를 사용하는 것입니다. 아두이노 코드 내에서 아래와 같이 사용할 수 있습니다.

```Arduino
String name = "John";
String text = "Hello, my name is %s"; 
String result = text.format(name);
```

위 코드에서 `%s`는 문자열 보간의 대상이 되는 변수를 나타냅니다. `format()` 함수를 사용하여 `text` 문자열 내에 있는 `%s` 자리에 `name` 변수의 값을 삽입한 뒤 `result` 변수에 저장합니다.

두 번째 방법은 `String` 클래스의 `operator+`을 이용하는 것입니다. 아래와 같이 사용할 수 있습니다.

```Arduino
String name = "John";
String text = "Hello, my name is " + name + "!";
```

위 코드에서 `+` 연산자는 문자열을 결합하는 용도로 사용되며, `name` 변수의 값을 `text` 문자열 뒤에 추가해주는 역할을 합니다.

아두이노에서 문자열을 처리할 때는 `String` 클래스 외에도 `char` 배열로 문자열을 처리하는 방법도 있습니다. 하지만 이 방법은 좀 더 복잡하고 번거로우므로 일반적으로 `String` 클래스를 사용하는 것이 좋습니다.

# Deep Dive:
문자열 보간에 대해 더 알아볼까요?

문자열 보간은 여러 개의 변수를 한 번에 삽입하는 것을 허용하여 코드를 간결하게 만드는 장점이 있습니다. 이 방법은 최근 프로그래밍 언어들에서 지원하는 기능이지만, 과거에는 수동으로 문자열을 연결해야 했기 때문에 코드가 복잡해지고 번거로웠습니다.

대부분의 프로그래밍 언어에서는 문자열 보간을 제공하며, 아두이노에서도 `String` 클래스를 이용하여 문자열 보간을 쉽게 사용할 수 있습니다.

하지만 문자열 보간이 항상 좋은 방법은 아닙니다. 예를 들어, 문자열 내에 여러 줄의 코드를 삽입하는 등 복잡한 기능을 구현하기에는 제한적일 수 있습니다. 이런 경우에는 문자열 병합(`String` 클래스의 `operator+`)을 사용하는 것이 더 적합할 수 있습니다.

# See Also:
관련 자료를 살펴볼까요?

- [The String data type in Arduino reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/) : 아두이노에서 문자열을 다루는 방법에 대한 공식 문서입니다.
- [String concatenation vs string substitution](https://stackoverflow.com/questions/5687292/string-concatenation-vs-string-substitution) : 문자열 보간과 문자열 병합을 비교하는 스택오버플로우 질문입니다.