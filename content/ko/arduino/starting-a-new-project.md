---
title:                "새 프로젝트를 시작하기"
html_title:           "Arduino: 새 프로젝트를 시작하기"
simple_title:         "새 프로젝트를 시작하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

# 무엇이고 왜?

새로운 프로젝트를 시작하는 것은 프로그래머들이 자신의 아이디어를 현실로 만들기 위해 시작하는 것입니다. 새로운 아이디어를 실현시키기 위해서는 새로운 프로젝트를 시작하는 것이 필수적입니다.

## 어떻게:

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

위의 코드는 내장 LED를 1초 간격으로 켜고 끄는 간단한 예시입니다. 이처럼 아두이노에서는 함께 사용할 수 있는 다양한 센서와 모듈들을 쉽게 조합하여 원하는 기능을 구현할 수 있습니다.

## 깊게 들어가기:

아두이노는 2005년 이탈리아의 하드웨어 디자이너 마스시모 베르누가 만든 오픈 소스 하드웨어 플랫폼입니다. 아두이노는 프로토타이핑과 인터랙티브 제품 개발에 최적화되어 있으며 현재 많은 사용자들에게 인기가 있습니다.

아두이노 외에도 라즈베리 파이와 같은 다른 싱글보드 컴퓨터들도 프로토타이핑 및 프로젝트 개발에 널리 사용되지만, 아두이노는 더 간단한 코딩 방식과 다양한 확장 가능성 때문에 초보자들에게 더 적합합니다.

아두이노는 C와 C++을 기반으로 하며, 아이디어를 실제로 구현하기 위해서는 기본적인 프로그래밍 지식이 필요합니다. 하지만 아두이노 커뮤니티에서는 다양한 예제와 도움말을 제공하여 쉽게 배우고 활용할 수 있도록 도와줍니다.

## 더 알아보기:

- 아두이노 공식 홈페이지: https://www.arduino.cc/
- 아두이노 커뮤니티: https://forum.arduino.cc/
- 아두이노 위키: https://en.wikipedia.org/wiki/Arduino