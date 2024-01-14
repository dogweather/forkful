---
title:    "Arduino: 문자열을 소문자로 변환하기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것이 중요한 이유는 문자열을 처리할 때 일관성을 유지하고 에러를 방지하기 위해서입니다. 데이터를 처리하거나 저장할 때 모두 소문자로 변환하여 일관성 있게 유지하는 것이 좋습니다.

## 방법

아두이노에서 문자열을 소문자로 변환하는 방법은 간단합니다. 다음과 같은 코드를 사용하면 됩니다.

```Arduino
 String str = "ARDUINO";
 Serial.println(str.toLowerCase());
```

위 코드를 실행하면 `arduino`라는 결과가 출력됩니다. 이와 같이 `toLowerCase()` 메소드를 사용하면 원본 문자열을 소문자로 변환할 수 있습니다.

## 깊게 들어가기

문자열을 소문자로 변환하는 과정에서 발생할 수 있는 알고리즘이나 이슈에 대해 더 깊게 알아보겠습니다.

### 알고리즘

보통 영어 알파벳의 ASCII 코드를 기준으로 대문자와 소문자는 마지막 bit가 다릅니다. 따라서 문자열을 순회하면서 해당 bit만 바꿔주면 소문자로 변환이 가능합니다.

### 이슈

하지만 이와 같은 간단한 알고리즘에서도 특수문자 (예: `Ü`)나 유니코드 문자들과 같은 다른 언어의 문자들을 처리하는 데 어려움이 있습니다. 따라서 이에 대한 처리를 생각해줘야 합니다.

## 더 알아보기

- [C++에서 문자열을 소문자로 변환하는 방법](https://www.geeksforgeeks.org/conversion-whole-string-uppercase-lowercase-using-stl-c/)
- [ASCII 코드에 대한 자세한 정보](https://ko.wikipedia.org/wiki/ASCII)
- [유니코드에 대한 정보](https://ko.wikipedia.org/wiki/%EC%9C%A0%EB%8B%88%EC%BD%94%EB%93%9C)