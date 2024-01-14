---
title:                "Arduino: 부분 문자열 추출하기"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

왜 누군가가 자르기를 통해 부분 문자열을 추출하는 데 참여할까요? 부분 문자열을 추출하는 것은 문자열에서 필요한 정보를 추출하는 간단하고 유용한 방법입니다. 이를 통해 문자열을 더 쉽게 다룰 수 있으며, 데이터를 분석하거나 처리할 때 유용하게 사용할 수 있습니다.

## 하는 법

우선, 추출하려는 문자열을 변수에 할당해야 합니다. 예를 들어, "Hello World"라는 문자열을 변수에 할당하고 싶다면 다음과 같이 합니다.

```Arduino
String str = "Hello World";
```

이제 우리는 문자열에서 원하는 부분을 추출할 수 있습니다. 부분 문자열을 추출하는 방법에는 여러 가지가 있지만, 여기서는 `substring()` 함수를 사용하겠습니다. 이 함수는 문자열에서 원하는 글자 수만큼의 부분 문자열을 추출해줍니다.

다음은 `substring()` 함수를 사용하여 "Hello"라는 부분 문자열을 추출하는 예시 코드입니다.

```Arduino
String str = "Hello World";
String sub = str.substring(0, 5);
```

위 코드에서 `substring()` 함수는 첫 번째 매개변수로 부분 문자열의 시작 인덱스, 두 번째 매개변수로 부분 문자열의 끝 인덱스를 받습니다. 따라서 위 코드는 "Hello World"라는 문자열에서 첫 5글자를 추출하여 `sub` 변수에 할당하는 것입니다.

만약 우리가 "World"라는 부분 문자열을 추출하고 싶다면 어떻게 할까요? 이 경우에는 두 번째 매개변수에 원하는 부분 문자열의 마지막 인덱스를 지정하면 됩니다.

```Arduino
String str = "Hello World";
String sub = str.substring(6, 11);
```

위 코드는 "Hello World"라는 문자열에서 인덱스 6부터 11까지의 문자열, 즉 "World"를 추출해줍니다.

이처럼 `substring()` 함수를 사용하면 문자열에서 필요한 부분을 쉽게 추출할 수 있습니다.

## 깊이 들어가기

`substring()` 함수를 사용하는 방법을 좀 더 깊이 들어가 보겠습니다. 이 함수는 세 개의 매개변수를 받을 수도 있습니다. 세 번째 매개변수를 사용하면 추출한 부분 문자열에서 원하는 문자를 제거할 수 있습니다.

예를 들어, "Hello World"라는 문자열에서 공백을 제거하고 싶다면 다음과 같이 `substring()` 함수를 호출하면 됩니다.

```Arduino
String str = "Hello World";
String sub = str.substring(0, 5, 0);
```

세 번째 매개변수는 제거할 문자의 개수를 나타내며, 위 코드에서는 0을 지정하여 공백을 모두 제거한 결과인 "HelloWorld"를 추출합니다.

또한 `substring()` 함수를 사용하여 추출한 부분 문자열을 다른 변수에 할당할 수도 있습니다. 이렇게 하면 추출한 문자열을 나중에 다시 사용할 수 있습니다.

```Arduino
String str = "Hello World";
String sub = str.substring(0, 5);
String sub2 = sub + " Universe";
```

위 코드에서는 먼저 "Hello World" 문자열에서 첫 5글자를 추출하여 `sub` 변수에 할당하고, `sub` 변수의 값에 " Universe" 문자열을 추가하여 `sub2` 변수에 할당하는 것입니다.

추출한 부분 문자열을 바로 사용하지 않고 나중에 사용하기 위해 변수