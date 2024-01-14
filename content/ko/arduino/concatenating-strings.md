---
title:    "Arduino: 문자열 연결하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

스트링 연결을 수행하는 이유는 여러 개의 문자열을 하나로 합치기 위해서입니다. 이를 통해 더 큰 문장을 만들 수 있으며, 다양한 문자열 조합을 통해 더 다양한 기능을 만들 수 있습니다.

## 코딩하기

스트링 연결은 간단한 코드로도 가능합니다. 아래의 예제 코드를 살펴보세요.

```Arduino
String name = "길동";
String language = "아두이노";
String introduction = "안녕하세요, 제 이름은 " + name + "이고, 저는 " + language + "를 배우고 있습니다.";
Serial.println(introduction);
```

코드의 결과는 다음과 같이 출력됩니다.

```
안녕하세요, 제 이름은 길동이고, 저는 아두이노를 배우고 있습니다.
```

## 심층적으로 살펴보기

스트링 연결을 할 때에는 몇 가지 주의할 점이 있습니다. 첫 번째는 데이터 타입을 일치시켜야 한다는 점입니다. 예를 들어, 숫자를 스트링과 연결할 경우 숫자를 스트링으로 변환해야 합니다. 또한, 많은 스트링을 연결할 경우 메모리를 많이 사용하게 되어 성능이 저하될 수 있습니다.

더 자세한 정보를 원하신다면, 아래의 링크들을 확인해보세요.

## 관련 링크

- [Arduino String 연산자](https://www.arduino.cc/reference/en/language/functions/communication/serial/)

- [Arduino Forum의 스트링 연산에 관한 토론](https://forum.arduino.cc/index.php?topic=526899.0)

- [스택오버플로우의 "아두이노를 사용하여 스트링 연결하기"](https://stackoverflow.com/questions/36915696/concatenate-strings-using-arduino)