---
title:                "새로운 프로젝트 시작하기"
html_title:           "Arduino: 새로운 프로젝트 시작하기"
simple_title:         "새로운 프로젝트 시작하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜 시작해야 하는가?

새로운 프로젝트를 시작하는 이유는 여러 가지이지만, 주로 그것을 통해 새로운 기술을 배우거나 프로그래밍 실력을 향상시키고자 할 때입니다. 또한 자신의 아이디어를 현실화시킬 수 있는 재미와 성취감을 얻을 수 있습니다.

## 시작하기

본격적으로 아두이노 프로그래밍을 시작하기 전에 몇 가지 준비물이 필요합니다. 먼저, 아두이노 보드와 USB 케이블이 필요합니다. 또한 컴퓨터에 아두이노 IDE를 설치해야 합니다. 모든 준비가 완료되었다면 아래의 코드 블록을 통해 실제 코딩을 시작해보세요.

```Arduino
void setup() {
  pinMode(13, OUTPUT);    // Pin 13을 출력 모드로 설정
}

void loop() {
  digitalWrite(13, HIGH); // Pin 13에 전압 공급
  delay(1000);            // 1초간 대기
  digitalWrite(13, LOW);  // Pin 13에 전압 차단
  delay(1000);            // 1초간 대기
}
```

위의 코드는 아두이노의 디지털 핀 13을 통해 LED를 깜빡이는 간단한 프로그램입니다. 이 코드를 실행하면 LED가 1초 간격으로 켜졌다가 꺼지는 것을 확인할 수 있습니다.

## 더 깊이 들어가기

새로운 프로젝트를 시작하려면 먼저 목표를 설정해야 합니다. 아두이노를 사용하여 무엇을 만들고 싶은지 정확히 결정하고, 그에 따라 필요한 하드웨어와 소프트웨어를 준비해야 합니다. 또한 아두이노의 특징과 주의할 점을 잘 숙지하여 원활한 프로젝트 진행에 도움이 될 수 있습니다.

## 관련 자료

- [아두이노 공식 사이트](https://www.arduino.cc/)
- [아두이노 우리말 위키](https://namu.wiki/w/아두이노)