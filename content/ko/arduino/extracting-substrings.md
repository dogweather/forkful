---
title:    "Arduino: 부분 문자열 추출하기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 왜

Arduino 프로그래밍에 대해 조금이라도 알고 계신 분들은 문자열을 다루는 것에 익숙하실 것입니다. 하지만 문자열 중에서도 특정 부분만을 추출하거나 분리해야 할 때가 있습니다. 이를 위해 문자열의 일부분을 추출하는 방법을 배워보겠습니다.

## 어떻게

먼저, `substring()` 함수를 사용하여 문자열의 일부분을 추출할 수 있습니다. 해당 함수는 문자열을 구성하는 문자들 중에서 일부를 선택하여 새로운 문자열을 만들어 줍니다. 아래의 예시를 살펴보세요.

```Arduino
String myString = "Hello World";
String substr = myString.substring(6,11);
// substr 값은 "World"가 됩니다.
```

`substring()` 함수의 첫 번째 파라미터는 문자열에서 추출하려는 시작 인덱스를 나타내며, 두 번째 파라미터는 추출하려는 마지막 인덱스 다음을 가리킵니다. 또한 첫 번째 인덱스는 0부터 시작하며, 마지막 인덱스는 실제 문자열 길이보다 1이 작아야 합니다.

더 복잡한 예시를 살펴보겠습니다.

```Arduino
String sensorData = "1023,428,512";
String data1 = sensorData.substring(0, sensorData.indexOf(','));
// data1 값은 "1023"이 됩니다.
String data2 = sensorData.substring(sensorData.indexOf(',') + 1, sensorData.lastIndexOf(','));
// data2 값은 "428"이 됩니다.
String data3 = sensorData.substring(sensorData.lastIndexOf(',') + 1);
// data3 값은 "512"가 됩니다.
```

위의 예시에서, `indexOf()` 함수는 해당 문자열에서 특정 문자의 인덱스를 찾아주며, `lastIndexOf()` 함수는 해당 문자열에서 마지막으로 등장하는 특정 문자의 인덱스를 찾아줍니다.

## 깊이 파헤치기

`substring()` 함수를 적절하게 사용하면, 문자열에서 원하는 부분을 추출하거나 분리할 수 있습니다. 또한 `indexOf()`와 `lastIndexOf()` 함수를 함께 사용하여 문자열에서 여러 구분자를 기준으로 추출할 수도 있습니다. 이외에도 `substring()` 함수는 문자열을 분리하는 데 유용하게 사용될 수 있습니다.

## 더 알아보기 

 * [String 객체의 substring() 함수 문서](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
 * [indexOf() 함수 문서](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)
 * [lastIndexOf() 함수 문서](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/lastindexof/)