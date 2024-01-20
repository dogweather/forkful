---
title:                "테스트 작성하기"
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-tests.md"
---

{{< edit_this_page >}}

# 무엇인고, 왜 그것을 하는가?
테스트 작성이란 무엇인가요? 이것은 프로그래머들이 왜 그것을 하는지에 대한 것입니다. 간단히 말하면, 테스트 작성은 소프트웨어 코드의 오류를 찾기 위한 과정입니다. 모든 프로그래머들은 완벽하지 않기 때문에 이 오류를 찾기 위해 테스트 작성은 필수적입니다.

# 방법:
아래의 코드 블록을 사용하여 샘플 출력과 함께 코딩 예제를 제공합니다. 아두이노를 사용하여 LED를 켜는 간단한 예제를 보여드립니다.

```Arduino
void setup() {
  pinMode(LED_BUILTIN, OUTPUT);     // LED pin을 출력으로 설정
}

void loop() {
  digitalWrite(LED_BUILTIN, HIGH);  // LED를 켬
  delay(1000);                      // 1초 동안 대기
  digitalWrite(LED_BUILTIN, LOW);   // LED를 끔
  delay(1000);                      // 1초 동안 대기
}
```

## 정식으로:
테스트 작성은 과거부터 프로그래머들이 사용해온 디버깅 방법입니다. 이후에는 다양한 방법들이 등장하였지만, 여전히 테스트 작성은 소프트웨어 개발에서 가장 중요한 부분 중 하나입니다. 대안으로는 디버깅 레거시와 같은 다른 도구들이 있지만, 이들은 프로그래머들의 생산성을 떨어뜨리기 때문에 최신 프로그래밍 언어에서는 테스트 작성을 지원합니다. 아두이노에서 테스트를 작성하는 방법은 다음과 같이 세 가지 중요한 단계로 이루어집니다.

## 참고자료:
아래의 링크는 프로그램 설치 및 사용에 도움이 될 수 있습니다.
- [아두이노 공식 홈페이지](https://www.arduino.cc)