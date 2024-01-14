---
title:    "Arduino: 패턴에 일치하는 문자 삭제하기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

Arduino 프로그래밍을 하는 동안 캐릭터를 삭제하는 것이 중요한 이유는 여러 가지 있습니다. 예를 들어, 우리는 입력된 데이터를 정제하거나 정렬하는 등의 목적으로 캐릭터를 삭제합니다. 혹은 우리가 만든 프로그램이 정해진 패턴의 문자를 제외한 다른 문자를 받아들이지 않도록 하기 위해서도 캐릭터 삭제를 하는 것이 중요할 수 있습니다.

## 하기

우선, 삭제하고자 하는 패턴을 정의해야 합니다. 이후에는 ```if```문을 사용하여 입력된 문자가 해당 패턴과 일치하는지를 확인합니다. 일치하지 않는 경우 해당 캐릭터를 출력하거나 다른 작업을 수행할 수 있습니다. 아래 예시 코드를 참고해주세요.

```Arduino
char input[] = "Hello123$&";
char pattern[] = "123$&";

//반복문을 통해 입력된 문자를 하나씩 확인합니다.
for(int i = 0; input[i] != '\0'; i++){
    //만약 입력된 문자가 패턴과 일치하지 않는 경우, 해당 문자를 출력합니다.
    if(input[i] != pattern[i]){
        Serial.println(input[i]);
    }
}
```

위 코드를 실행하면 ```Hello```가 출력될 것이고, 패턴과 일치하지 않는 ```123$&```는 출력되지 않을 것입니다. 이렇게 함으로써 우리는 정해진 패턴의 캐릭터를 삭제할 수 있게 됩니다.

## 깊게 파헤치기

삭제하고자 하는 패턴이 복잡하거나 여러 개의 패턴에 대해 적용해야 할 경우에는 어떻게 해야 할까요? 이런 경우에는 정규표현식을 사용할 수 있습니다. 정규표현식은 복잡한 패턴을 간단하게 표현할 수 있게 해주는 도구입니다. 아래 예시 코드를 참고해주세요.

```Arduino
#include <regex.h>

char input[] = "Hello123$& World456^";

// 패턴 "Hello"와 "World"의 캐릭터를 제외한 모든 캐릭터를 삭제하기 위한 정규표현식입니다.
char pattern[] = "[Hello]|[World]"; 

//정규표현식을 사용하기 위해 필요한 객체를 생성합니다.
regex_t regex;

//패턴을 컴파일합니다. 성공적인 경우 0을 반환합니다.
int result = regcomp(&regex, pattern, 0);

//만약 컴파일에 실패한 경우 에러 메시지를 출력합니다.
if(result != 0){
    char error_message[100];
    regerror(result, &regex, error_message, sizeof(error_message));
    Serial.println(error_message);
}

//패턴과 일치하지 않는 캐릭터를 삭제합니다.
char new_string[100];
regreplace(new_string, input, &regex, "");
Serial.println(new_string);
```

위 코드를 실행하면 ```Hello World```만 남게 될 것입니다. 이처럼 정규표현식을 사용하면 복잡한 패턴을 효과적으로 삭제할 수 있습니다.

## 이어서 보기

- [](https://www.arduino.cc/en/Reference/String)
- [](https://www.geeksforgeeks.org/regular-expressions-in-c-set-1-introduction-basics/)