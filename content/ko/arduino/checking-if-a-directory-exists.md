---
title:    "Arduino: 프로그래밍에서 디렉토리가 존재하는지 확인하기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 왜
디렉토리가 존재하는지 확인하는 것은 프로그램 개발에 있어 중요한 부분입니다. 디렉토리가 존재하는지 여부를 확인하는 것은 데이터 파일과 같은 중요한 파일에 접근할 때 사용되며, 이를 통해 오류를 방지하고 안전성을 높일 수 있습니다.

## 어떻게
디렉토리가 존재하는지 확인하는 가장 간단한 방법은 아두이노의 File 객체를 사용하는 것입니다. 아래는 아두이노 보드에서 디렉토리가 존재하는지 여부를 확인하는 예제 코드와 출력 결과를 보여줍니다.

```Arduino 
#include <SPI.h>
#include <SD.h>

void setup() {
  pinMode(10, OUTPUT);
  if (!SD.begin(10)) {
    Serial.println("SD 카드를 읽지 못했습니다.");
    while (1);
  }
}

void loop() {
  if (SD.exists("data")) {
    Serial.println("data 디렉토리가 존재합니다.");
  } else {
    Serial.println("data 디렉토리가 존재하지 않습니다.");
  }
  delay(1000);
}
```

출력:

```
data 디렉토리가 존재합니다.
```

여기서 ```SD.begin(10)```은 SD 카드의 삽입 핀을 10번 핀으로 설정하는 부분입니다. 따라서 다른 핀을 사용하고 있다면 해당 핀으로 변경해주어야 합니다.

## 딥 다이브
```SD.exists()``` 함수는 디렉토리 뿐만 아니라 파일의 존재 여부도 확인할 수 있습니다. 또한, 아두이노에서는 SD 카드가 존재해도 해당 파일이나 디렉토리가 없는 경우에는 오류가 발생하지 않고 false를 반환합니다. 이를 이용하여 파일/디렉토리가 존재하는지 여부를 확인하고, 필요에 따라 오류를 처리할 수 있습니다.

## 이어서 보기
* [Arduino SD 라이브러리 공식 문서](https://www.arduino.cc/en/Reference/SD)
* [SD 라이브러리 예제 코드](https://startiot.tistory.com/entry/Arduino-SD-%EC%82%AC%EC%9A%A9) 
* [아두이노에서 SD 카드 사용하기](https://blog.naver.com/kih03011/220752167451)