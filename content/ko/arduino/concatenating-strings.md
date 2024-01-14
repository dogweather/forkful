---
title:                "Arduino: 문자열 연결하기"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# 왜?
문자열 연결(concatenating strings)을 사용하는 이유는 여러 개의 문자열을 하나의 문자열로 결합하여 보다 복잡한 프로그래밍을 가능하게 하기 위해서입니다.

## 어떻게?
Arduino에서 문자열을 연결하는 방법은 매우 간단합니다. 아래 예시를 참고해보세요. 

```Arduino
String firstName = "Erica";
String lastName = "Park";
String fullName = firstName + " " + lastName;
Serial.println(fullName);
```

위 코드는 "Erica Park"을 출력합니다. 여기서는 세 가지 문자열을 `+` 연산자를 사용하여 연결했습니다. 문자열을 연결할 때는 항상 "+" 연산자를 사용해야합니다.

## 딥 다이브
문자열을 연결하는 방법은 매우 간단하지만, 연산량에 대해서는 주의해야합니다. 문자열을 연결할 때마다 새로운 문자열 객체가 생성되기 때문에 메모리 사용량이 증가할 수 있습니다. 따라서 많은 문자열을 연결하는 경우, 메모리 관리에 유의해야 합니다.

## 또 다른 참고 자료
- [Arduino String 클래스에 대한 더 자세한 정보](http://arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [String 연산자에 대한 추가적인 정보](http://arduino.cc/reference/en/language/variables/data-types/string/operators/)
- [문자열 함수에 대한 설명서](http://arduino.cc/en/reference/string/functions)

# 자료 참고
- [Arduino String 클래스에 대한 더 자세한 정보](http://arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [문자열 함수에 대한 설명서](http://arduino.cc/en/reference/string/functions)
- [Arduino String 연산자에 대한 추가적인 정보](http://arduino.cc/reference/en/language/variables/data-types/string/operators/)