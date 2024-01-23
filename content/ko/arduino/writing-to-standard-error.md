---
title:                "표준 오류로 쓰기"
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
프로그램에서, 표준 오류(stdout)는 오류 메시지를 출력하는 특별한 경로다. 이를 사용해 문제 발생시 진단 정보를 제공하고, 정상 출력(stdout)과 구분한다.

## How to:
Arduino에서 표준 오류 스트림을 직접 쓰는것은 불가능하지만, Serial을 이용해 비슷한 기능을 구현할 수 있다. 아래 예시를 보자.

```Arduino
void setup() {
  Serial.begin(9600);  // 시작시 시리얼 통신 시작
}

void loop() {
  // 정상 메시지 출력
  Serial.println("Running...");

  // 에러 발생 가정
  bool errorOccurred = true; 
  if (errorOccurred) {
    Serial.println("ERROR: Something went wrong!"); // 에러 메시지 출력
  }

  delay(1000); // 1초마다 반복
}
```
출력 예시:
```
Running...
ERROR: Something went wrong!
Running...
...
```

## Deep Dive (심화 학습)
Arduino는 본래 마이크로컨트롤러용으로 간단한 I/O 작업에 초점을 맞추어져 있어, 어떤 개념들은 직접적인 대응이 없을 수 있다. 표준 오류에 직접 쓰기보다는 `Serial.print`나 `Serial.println`을 이용하여 디버깅 정보를 출력한다. 역사적으로, 표준 오류는 유닉스 시스템에서 프로그램의 오류를 처리하기 위해 분리된 출력 경로로 개발되었으며, 다양한 프로그래밍 환경에서 중요한 개념으로 자리 잡았다. 그러나, Arduino에서는 Serial 객체가 표준 출력과 비슷한 역할을 한다.

## See Also (추가 자료)
- Arduino Serial 참조: [Arduino - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- 표준 오류 컨텍스트에서 C++ 스트림 사용 방법: [cppreference - cerr](https://en.cppreference.com/w/cpp/io/cerr)
- 표준 I/O 스트림 역사적 배경: [The TTY demystified](http://www.linusakesson.net/programming/tty/index.php)
