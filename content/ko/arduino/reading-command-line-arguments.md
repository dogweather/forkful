---
title:    "Arduino: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 왜 Arduino 프로그래밍을 읽어야 하는지

커맨드 라인 인자를 읽는 것은 Arduino 프로그래밍에서 매우 유용합니다. 이 기능을 사용하면 사용자의 입력을 읽어 프로그램에 대해 더 많은 제어를 할 수 있습니다.

## 어떻게

아래의 예제 코드를 통해 커맨드 라인 인자를 읽는 방법을 알아보겠습니다. 이 코드는 사용자가 입력한 인자를 숫자로 변환하고, 그 값을 시리얼 모니터에 출력합니다.

```Arduino
int numberOne;
int numberTwo;

void setup() {
  Serial.begin(9600);     // 시리얼 통신 시작
  while (!Serial) {}      // 시리얼 통신이 연결될 때까지 대기
  numberOne = atoi(argv[1]);   // 첫 번째 인자를 정수로 변환하여 numberOne 변수에 저장
  numberTwo = atoi(argv[2]);   // 두 번째 인자를 정수로 변환하여 numberTwo 변수에 저장
  Serial.print("The sum of the two numbers is: ");    // 출력
  Serial.println(numberOne + numberTwo);    // 두 수의 합을 계산하여 출력
}

void loop() {

}
```

사용자가 아래와 같은 입력을 준다고 가정해 봅시다.

`123 456`

이 입력은 첫 번째 인자가 123 이고 두 번째 인자가 456 인 것을 의미합니다. 따라서 출력은 아래와 같을 것입니다.

`The sum of the two numbers is: 579`

## 더 깊게 들어가기

커맨드 라인 인자를 읽는 방법에 대해 더 자세히 알아보겠습니다. 먼저 `Serial.begin()` 함수를 통해 시리얼 통신을 시작합니다. 이 함수는 아두이노와 컴퓨터 간의 통신을 위한 설정을 해줍니다. 그 다음 `while` 루프를 사용하여 시리얼 통신이 연결될 때까지 대기합니다. 이렇게 함으로써 이후의 코드에서 시리얼 통신을 사용할 수 있게 됩니다.

`atoi()` 함수는 문자열을 정수로 변환해주는 역할을 합니다. 위의 예제에서는 두 번째 인자까지만 변환을 해주지만, 인자가 더 많을 수도 있습니다. 이 경우 `argc` 변수를 사용하여 인자의 개수를 알 수 있습니다. `argv` 변수는 각 인자를 배열로 저장하고 있습니다. 따라서 `argv[0]` 은 프로그램 이름, `argv[1]`은 첫 번째 인자, `argv[2]`는 두 번째 인자를 나타낼 것입니다.

이렇게 하여 커맨드 라인 인자를 읽고 사용자의 입력에 따라 프로그램에 더 많은 제어를 할 수 있게 됩니다.

## 더 알아보기

커맨드 라인 인자를 읽는 방법에 대해 더 자세히 알아보고 싶다면 아래의 링크를 참고해보세요.

[Arduino 공식 문서](https://www.arduino.cc/reference/en/language/functions/communication/serial/begin/)

[Serial 통신에 대한 자세한 설명](https://learn.sparkfun.com/tutorials/serial-communication/all)

# 더 알아보기

- [Arduino 공식 문서](https://www.arduino.cc/reference/en/language/functions/communication/serial/begin/)
- [Serial 통신에 대한 자세한 설명](https://learn.sparkfun.com/tutorials/serial-communication/all)