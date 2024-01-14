---
title:                "Arduino: 컴퓨터 프로그래밍에서 명령 줄 인자 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령 줄 인자 읽기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인수를 읽는 방법을 배우는 것은 아두이노 프로그래밍을 더욱 유연하고 효율적으로 만들 수 있기 때문에 중요합니다.

## 어떻게

다음 예제 코드를 사용하여 커맨드 라인 인수를 읽는 방법을 배워보세요.

```Arduino
String input = "";

void setup() {
  Serial.begin(9600);
  while (! Serial) {
    ; // wait for serial port to connect. Needed for native USB port only
  }

  // read input from command line
  if (Serial.available()) {
    input = Serial.readString();
  }

  Serial.print("Input: ");
  Serial.println(input);
}

void loop() {
  // do something with the input
}
```

예제 코드를 실행하면 시리얼 모니터 창에서 입력한 값을 출력할 수 있습니다.

```
Enter input: Hello world!
Input: Hello world!
```

## 깊이 알아보기

커맨드 라인 인수를 읽는 것은 멀티플랙스 프로그래밍에 유용할 수 있습니다. 예를 들어, 센서에서 읽은 값을 인수로 전달하여 원하는 작업을 수행할 수 있습니다.

## 또 다른 정보

- [아두이노 공식 사이트](https://www.arduino.cc/)
- [아두이노 코리아](https://www.arduinokorea.com/)
- [아두이노 포럼](https://forum.arduino.cc/index.php)