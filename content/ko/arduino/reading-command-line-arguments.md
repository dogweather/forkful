---
title:                "Command line 인수 읽기"
html_title:           "Arduino: Command line 인수 읽기"
simple_title:         "Command line 인수 읽기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜
프로그래밍에서 라인 인자(argument)를 읽는 것은 매우 유용합니다. 이 기술을 통해 사용자의 입력을 받고, 그에 따라 프로그램의 동작을 다르게 할 수 있습니다. 따라서 이 글을 통해 라인 인자를 읽는 방법을 배우게 됩니다.

## 사용 방법
먼저, 모든 인자(argument)를 받은 후 `for`문을 통해 각 인자를 순회합니다. 그리고 `Serial.println()`을 통해 각 인자를 출력합니다.

```Arduino
for (int i = 0; i < commandLineArguments.length(); i++){ 
  Serial.println(commandLineArguments[i]);
}
```

예를 들어 `LED`를 켜는 프로그램에서, `arduino.exe on`이라는 명령어를 입력하면 `commandLineArguments` 배열에는 `arduino.exe`와 `on`이 각각 저장됩니다. 이러한 방식으로 인자를 읽어서 프로그램의 동작을 제어할 수 있습니다.

## 심화 공부
라인 인자를 읽기 전, `Serial.begin(9600)`을 통해 시리얼 통신을 시작해야 합니다. 그리고 `Serial.readString()`을 통해 사용자의 입력을 받습니다. 이렇게 받은 문자열을 `split()` 함수를 이용해 공백을 기준으로 나눈 후, 각각을 `commandLineArguments` 배열에 저장하는 방식으로 라인 인자를 읽을 수 있습니다.

See Also
- [Serial Interface for Arduino](https://create.arduino.cc/projecthub/Arduino_Genuino/serial-interface-with-arduino-3cdc4d)
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino String Split() Function](https://www.arduino.cc/en/Reference/SerialSplit)