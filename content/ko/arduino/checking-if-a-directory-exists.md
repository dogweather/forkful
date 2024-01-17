---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Arduino: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

개발자들은 때때로 프로그램 내에서 디렉토리가 존재하는지 확인하는 것이 필요합니다. 이를테면 파일을 읽거나 쓰기 전에 해당 디렉토리의 유효성을 검사하는 등의 경우입니다. 이를테면 프로그램이 오류 없이 실행되는지 보장하고, 불필요한 예외 상황을 방지하기 위해 디렉토리가 있는지 확인합니다.

## 어떻게:

디렉토리의 존재 여부를 확인하는 가장 간단한 방법은 ```exists()``` 함수를 사용하는 것입니다. 이 함수는 디렉토리 객체를 인자로 받고, 디렉토리가 존재하면 ```true```를 반환하고 아니라면 ```false```를 반환합니다. 아래의 예시 코드를 참고해보세요.

```Arduino
#include <SD.h> //SD 라이브러리 불러오기

void setup() {
  Serial.begin(9600); //시리얼 통신 설정
  if (SD.exists("/myFolder")) { //디렉토리의 존재 여부 확인
    Serial.println("디렉토리가 존재합니다."); //디렉토리가 존재할 경우 출력
  }
  else {
    Serial.println("디렉토리가 존재하지 않습니다."); //디렉토리가 존재하지 않을 경우 출력
  }
}

void loop() {
  //아무 작업 하지 않음
}
```

위의 코드를 실행하면 시리얼 모니터에 디렉토리의 존재 여부가 출력됩니다.

## 깊이 들어가보기:

디렉토리의 존재 여부를 확인하기 위해서는 SD 라이브러리를 사용해야 합니다. 이 라이브러리는 아두이노에서 SD 카드와 관련된 작업을 할 수 있도록 도와줍니다. 만약 SD 카드가 연결되어 있지 않다면, ```exists()``` 함수는 항상 ```false```를 반환할 것입니다.

## 관련 자료:

- [SD 라이브러리 공식 문서](https://www.arduino.cc/en/Reference/SD)
- [SD 카드가 아닌 다른 디렉토리를 확인하는 방법](https://arduino.stackexchange.com/questions/35848/check-if-a-directory-exists-not-check-a-folder)