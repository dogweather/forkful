---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

새 프로젝트를 시작하는 것은 개발자가 아이디어를 현실로 구현하는 첫 단계입니다. 이를 통해 프로그래머들은 창의적인 생각을 형상화하고 세계를 바꿀 수 있습니다.

## 어떻게 하는가:

아래의 코드는 LED가 1초 간격으로 깜박이는 아두이노 프로젝트를 시작하는 예제입니다.

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
이 코드를 업로드하면 내장 LED가 1초 간격으로 켜지고 꺼집니다.

## 딥 다이브:

1. **역사적 맥락:** 아두이노는 2005년 이탈리아의 Interaction Design Institute에서 학생들에게 프로그래밍 및 전자공학을 손쉽게 가르치기 위한 도구로 개발되었습니다.
  
2. **대안들:** 라즈베리 파이, ESP8266 등 다른 마이크로 컨트롤러도 고려해 볼 만한 대안입니다. 그러나 아두이노는 입문자 친화적인 커뮤니티와 일관된 업데이트로 인해 여전히 인기가 있습니다.
   
3. **구현 세부사항:** 새 프로젝트를 시작하면, 아두이노 IDE는 기본 포맷인 `setup()`와 `loop()` 함수를 생성합니다. `setup()`는 프로그램이 시작할 때 한 번만 실행되고, `loop()`는 계속해서 반복됩니다.

## 참고 자료:

- [아두이노 공식 웹사이트](https://www.arduino.cc/)
- [아두이노를 위한 프로젝트 아이디어](https://create.arduino.cc/projecthub)
- [LED 깜박이기 튜토리얼](https://www.arduino.cc/en/Tutorial/BuiltInExamples/Blink)