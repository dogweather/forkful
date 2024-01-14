---
title:                "Arduino: 문자열의 길이 찾기"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

아두이노 프로그래밍을 시작하면 아마도 작업하는 동안 문자열의 길이를 파악해야할 때가 있을 것입니다. 이 길이를 알지 못하면 필요한 작업을 해내기가 어려울 수 있습니다. 아두이노로 문자열 길이를 찾는 방법에 대해 알아보겠습니다.

## 방법

아두이노에서는 문자열 길이를 파악하기 위해 내장된 `strlen()` 함수를 사용할 수 있습니다. 이 함수는 문자열의 길이를 바이트 단위로 반환합니다. 이를테면, "Hello"라는 문자열은 5바이트로 이루어져 있으므로 `strlen("Hello")`를 호출하면 5가 출력됩니다.

아래의 예시를 참고해보세요.

```Arduino
char myString[] = "Hello";
int stringLength = strlen(myString);
Serial.print("The length of my string is: ");
Serial.println(stringLength); // 5가 출력됨
```

이번에는 어떻게 `for` 루프를 사용해 문자 하나하나를 세고, 문자열의 길이를 계산하는 방법을 알아보겠습니다.

```Arduino
char myString[] = "Hello";
int stringLength = 0;
for (int i = 0; myString[i] != '\0'; i++) {
  stringLength++;
}
Serial.print("The length of my string is: ");
Serial.println(stringLength); // 5가 출력됨
```

위의 두 예시는 모두 같은 결과를 출력하지만, `strlen()` 함수를 사용하는 것이 더 간단하고 효율적입니다.

## 깊게 들어가기

문자열의 길이를 찾는 작업은 프로그래밍에서 매우 기본적이고 필수적인 작업입니다. 이 작업이 왜 효율적인지 이해하려면 컴퓨터의 메모리 저장 구조를 알아야 합니다. 문자열은 문자들의 배열로 이루어져 있으며, 마지막으로는 `'\0'`(NULL)문자가 추가로 들어갑니다. `strlen()` 함수는 이 `'\0'` 문자를 찾아나가면서 문자열의 길이를 세어주는 것입니다.

이러한 작업의 반복 빈도가 높을 경우, `for` 루프를 사용하는 것보다는 `strlen()` 함수를 사용하는 것이 더 효율적입니다. 이는 `strlen()` 함수가 미리 최적화되어 있기 때문입니다.

## 참고

아두이노 공식 사이트의 `strlen()` 함수 문서 참고하기: https://www.arduino.cc/reference/en/language/functions/string/functions/strlen/
Stack Overflow의 비슷한 질문과 답변 참고하기: https://stackoverflow.com/questions/943538/q-how-do-i-determine-the-length-of-an-array-of-characters