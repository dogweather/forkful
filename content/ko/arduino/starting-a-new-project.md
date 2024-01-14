---
title:                "Arduino: 새 프로젝트 시작하기"
programming_language: "Arduino"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜

새로운 프로젝트를 시작하는 이유는 여러 가지가 있습니다. 어떤 사람들은 창조적인 발전을 위해, 어떤 사람들은 새로운 기술을 배우기 위해, 어떤 사람들은 취미 생활을 즐기기 위해 프로젝트를 시작합니다. 어떤 이유로든, 새로운 프로젝트를 시작하는 것은 즐거운 일입니다.

## 방법

새로운 프로젝트를 시작하기 위해 우선 Arduino를 우리의 컴퓨터에 설치해야 합니다. 그리고 아래의 코드를 따라서 우리의 첫번째 예제를 만들어 보겠습니다.

```Arduino
void setup() {
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);
  delay(1000);
  digitalWrite(LED_BUILTIN, LOW);
  delay(1000);
}
```

이 예제는 LED를 1초마다 켜고 끄는 간단한 프로그램입니다. 위의 코드를 복사하여 Arduino IDE에 붙여넣고, 네트워크의 아두이노 보드를 연결한 후 "업로드" 버튼을 눌러 우리의 보드에 코드를 업로드해보세요. 보드의 LED가 1초마다 깜박일 것입니다!

## 깊이 파고들기

새로운 프로젝트를 시작할 때 중요한 것은 우리가 창조적으로 생각하고, 독창적인 프로젝트를 만들 수 있게 도와주는 것입니다. Arduino는 많은 센서와 기기들을 지원하기 때문에 우리의 프로젝트를 다양하고 흥미롭게 만들어줄 수 있습니다. Arduino에서는 다른 라이브러리들을 사용하여 여러 가지 미디어 장치들을 연결할 수도 있습니다. 이를 통해 우리의 창의력을 발휘하여 더욱 다양하고 멋진 프로젝트를 만들어 낼 수 있습니다.

## 이것도 참고해보세요

- [Arduino 공식 웹사이트](https://www.arduino.cc/)
- [Arduino 예제 코드](https://www.arduino.cc/en/Tutorial/BuiltInExamples)
- [Arduino 포럼](https://forum.arduino.cc/)