---
title:    "Arduino: 문자열의 길이 찾기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜
아두이노를 사용하는 프로그래머들은 문자열의 길이를 찾는 것에 많은 관심을 가지고 있습니다. 이는 프로젝트에서 문자열을 다루기 위해 필요하기 때문입니다.

## 어떻게
문자열의 길이를 찾는 방법은 매우 간단합니다. 아두이노에서는 `strlen()` 함수를 사용하면 됩니다. 다음은 `strlen()` 함수를 사용한 예제 코드와 출력 결과입니다.

```Arduino
// 문자열 선언
String myString = "Hello World!";
// 문자열의 길이를 찾아 출력
Serial.println(strlen(myString));
```

출력 결과:
```
12
```

## 깊이 파고들기
`strlen()` 함수는 문자열의 길이를 찾기 위해 각 문자를 하나씩 검사합니다. 문자열의 끝을 나타내는 널 문자(`'\0'`)를 만나면 검사를 멈춥니다. 따라서 `strlen()` 함수는 널 문자가 저장된 메모리 주소와 문자열의 첫 번째 문자의 메모리 주소의 차이를 계산하여 길이를 반환합니다.

## 함께 보기
- [C언어 포인터와 문자열](https://codedragon.tistory.com/6770)
- [C언어 문자열의 길이를 찾는 방법](https://coding-factory.tistory.com/548)
- [아두이노와 문자열의 다양한 사용 예시](https://www.arduino.cc/en/Tutorial/StringIndexOf)