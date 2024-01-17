---
title:                "표준 에러 작성"
html_title:           "Arduino: 표준 에러 작성"
simple_title:         "표준 에러 작성"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 뭐랑 왜? 
Arduino 프로그래머들은 보통 "standard error"에 쓰는 걸 좋아합니다. 이걸 쓰는 이유는 뭔데? 네가 에러 메시지를 더 잘 이해할 수 있게 도와주기 때문입니다. 예를 들면, 네가 코드를 작성하면서 실수가 생기면, 에러 메시지가 나오고 그게 네가 어디서 실수를 했는지 알려주고 어떻게 고칠 수 있는지도 알려줘. 코드를 디버깅할 때 정말 유용하죠.

## 어떻게?
보통, 너가 Arduino 코드를 작성할 때, 너는 `Serial.println()`을 써서 시리얼 통신을 할 수 있어. 이 기능을 이용해서 에러 메시지를 출력할 수도 있어. 아래 코드를 한 번 봐봐.

```
void setup() {
  Serial.begin(9600); //시리얼 통신 시작
}

void loop() {
  //여기에서 코드를 작성해
  Serial.println("에러 발생!"); // 에러 메시지 출력
}
```

이 코드를 실행하면, 아래와 같은 메시지가 시리얼 모니터에 출력되겠지?

```
에러 발생!
```

이렇게 하면, 너가 어디서 실수를 했는지 알 수 있고, 그 에러를 고칠 수도 있다는 거야. 손쉽게 디버깅을 할 수 있게 되는거지.

## 깊이 파고들기 
`Serial.println()`은 Arduino를 쓰기 전부터 있었던 기능이야. 그런데 이걸 쓰기 전에는 애러 메세지를 출력하기가 좀 귀찮았거든. 예를 들면, `digitalWrite()` 함수를 이용해서 핀에 값을 쓸 때, 그 핀을 OUTPUT으로 설정하지 않으면 에러 메세지가 뜨는 거야. 그리고 그 에러 메세지는 시리얼 통신 없이는 볼 수 없었기 때문에 실수할 때마다 USB 케이블을 뽑고 다시 연결해야 했어. 너무 번거롭지 않아? 그래서 Arduino 개발자들은 `Serial.println()`을 도입하게 되었어. 이제 이 기능을 이용해서 에러 메세지를 볼 수 있게 되었어. 

또 다른 대안으로는 `Serial.print()`를 사용하는 것이 있어. 이 함수는 `Serial.println()`과 비슷하지만 개행 문자(새로운 줄로 이동)를 출력하지 않는거야. 그래서 메시지를 출력하고 나서도 시리얼 모니터는 같은 줄에 남아있게 돼. 간단한 코드에서는 비슷하지만, 더 복잡한 경우에는 `Serial.print()`를 사용하는 것이 더 좋을 수도 있어.

## 더 참고하기 
- [Arduino Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Serial.println() vs Serial.print()](https://stackoverflow.com/questions/40719427/serial-println-vs-serial-print-arduino)
- [Debugging Arduino with Serial.print()](https://create.arduino.cc/projecthub/27c/making-sense-of-arduino-serial-ports-fd1f1a)