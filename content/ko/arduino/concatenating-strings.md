---
title:    "Arduino: 문자열 결합"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것에 참여하는 이유는 여러가지가 있습니다. 예를 들어, 여러 개의 변수를 한 번에 출력해야 할 때나, 문자열로 표시된 정보를 정렬해서 사용할 때가 있습니다.

## 하우 투

본문에서는 문자열을 연결하는 여러 가지 방법을 소개하겠습니다. 우선, **+** 연산자를 사용하는 방법부터 살펴보겠습니다.

```Arduino
String first = "Hello";
String second = "World";
String result = first + second;
```

위 코드에서 result 변수에는 "HelloWorld"가 저장됩니다. 이는 각각의 문자열을 더하는 것과 동일한 결과를 가져옵니다.

또한, Arduino에서는 **concat()** 함수를 사용하여 문자열을 연결할 수 있습니다.

```Arduino
String first = "Hello";
String second = "World";
first.concat(second);
```

위 코드에서 first 변수에는 "HelloWorld"가 저장됩니다. 이는 first 변수에 second 변수를 연결하는 것과 동일한 결과를 가져옵니다. 이 방법은 첫 번째 문자열에 두 번째 문자열을 연결하는 것이기 때문에, 첫 번째 문자열에 값을 바로 연결하는 것보다 효율적입니다.

또한, 여러 개의 변수를 한 번에 연결할 수도 있습니다.

```Arduino
String first = "Hello";
String second = "World";
String third = "!";
first = first + second + third;
```

위 코드에서 first 변수에는 "HelloWorld!"가 저장됩니다. 이렇게 여러 개의 변수를 한 번에 연결할 수 있습니다.

때로는 문자열과 숫자를 함께 연결해야 할 때도 있습니다. 이 경우에는 앞서 살펴본 방법과는 조금 다른 방법을 사용해야 합니다.

```Arduino
String text = "The value of number is: ";
int number = 5;
String result = text + String(number);
```

위 코드에서 result 변수에는 "The value of number is: 5"가 저장됩니다. 이는 text 문자열과 number 변수를 함께 연결한 결과입니다. 이를 위해 **String()** 함수를 사용하여 숫자를 문자열로 변환해야 합니다.

## 딥 다이브

문자열을 연결하는 것은 간단한 작업처럼 보일 수 있지만, 실제로는 문자에 대한 메모리 공간을 예약하고 변수를 연결하는 과정이 숨어있을 수 있습니다. 따라서 문자열을 많이 연결하게 되면 메모리 부족과 관련된 문제가 발생할 수 있습니다. 따라서 문자열을 연결할 때에는 신중하게 생각해야 합니다.

## 더 알아보기

아래의 링크들을 참고하여 더 많은 정보를 얻을 수 있습니다.

- [Arduino 공식 사이트](https://www.arduino.cc/)
- [Official Arduino Reference - String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [How to concatenate strings in Arduino](https://techtutorialsx.com/2016/10/15/arduino-how-to-concatenate-strings/)