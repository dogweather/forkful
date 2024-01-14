---
title:    "Arduino: 텍스트 파일 작성하기"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 것을 왜 해야할까요? 아두이노 프로그래밍에서 왜 중요한지 궁금하셨나요? 이 블로그 포스트에서 알려드리겠습니다.

## How To

텍스트 파일을 작성하는 방법을 알려드리겠습니다. 아래의 코드 블록과 샘플 출력을 참고하시면 더 쉽게 이해하실 수 있습니다.

```Arduino
#include <SPI.h>

File myFile;

void setup() {
  Serial.begin(9600); // 시리얼 통신 설정
  while(!Serial) {
    ; // 아두이노와 업로더가 연결될 때까지 대기
  }

  Serial.print("연결 준비 완료!");

  pinMode(10, OUTPUT); // 핀 10을 출력용으로 설정

  if (!SD.begin(4)) { // SD 카드 읽기 시작, 핀 4에 연결된 카드 리더 사용
    Serial.println("카드 오류");
    return;
  }

  Serial.println("카드 준비 완료");
}

void loop() {
  myFile = SD.open("test.txt", FILE_WRITE); // "test.txt"라는 이름의 파일을 쓰기 모드로 열기
  if (myFile) {
    myFile.println("Hello World!"); // 파일에 문자열 쓰기
    myFile.close(); // 파일 닫기
    digitalWrite(10, HIGH); // 파일 쓰기 성공 시 LED를 켜기
  } else {
    Serial.println("파일 오류");
    digitalWrite(10, LOW); // 파일 쓰기 실패 시 LED를 끄기
  }

  delay(100); // 0.1초 대기
}
```

```
연결 준비 완료!
카드 준비 완료
```

## Deep Dive

텍스트 파일은 정보를 보관하기 위한 가장 일반적인 형식입니다. 파일을 작성하는 방법은 다양하지만, 아두이노에서는 SD(메모리 카드)를 사용하여 파일을 쓰고 읽을 수 있습니다. 파일을 쓰려면 파일 이름과 함께 `FILE_WRITE` 모드를 사용하여 파일을 열고, `println()` 함수를 사용하여 문자열을 쓸 수 있습니다. 파일을 성공적으로 열고 쓴 후에는 `close()` 함수를 사용하여 파일을 닫아야 합니다. 파일을 열 수 없다면 SD 카드 연결 오류나 파일이 손상되었을 수 있습니다. 이러한 오류를 방지하려면 항상 파일 작성 전에 파일을 쓰기 모드로 열었는지 확인하는 것이 중요합니다.

## See Also

- [Arduino 공식 사이트](https://www.arduino.cc/)
- [SD 라이브러리 사용법](https://www.arduino.cc/en/Reference/SD)
- [SD 라이브러리 관련 예제](https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite)