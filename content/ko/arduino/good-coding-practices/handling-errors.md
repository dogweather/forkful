---
title:                "에러 처리하기"
aliases:
- /ko/arduino/handling-errors/
date:                  2024-01-26T00:50:13.043757-07:00
model:                 gpt-4-1106-preview
simple_title:         "에러 처리하기"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/handling-errors.md"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

프로그램에서의 에러 처리는 뜻하지 않게 여러분을 넘어뜨리려는 것들을 잡아내는 과정입니다. 예상치 못한 일이 발생했을 때 아두이노가 멜트다운에 빠지지 않도록 하기 위해 이를 수행합니다.

## 어떻게 하나:

예를 들어, 가끔 범위를 벗어난 값을 생성할 수 있는 센서를 읽는 아두이노라고 해봅시다. 이를 처리하는 방법은 다음과 같습니다:

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // 값이 범위 안에 있으므로 처리를 계속 진행
  Serial.println(sensorValue);
} else {
  // 값이 범위를 벗어났으므로 에러 처리
  Serial.println("Error: Sensor value out of range.");
}
```
샘플 출력:
```
523
Error: Sensor value out of range.
761
```

## 심층 분석

에러 처리는 항상 이렇게 간단하지 않았습니다. 초기 개발자들은 종종 에러를 무시했고, 이로 인해 두려워하는 "정의되지 않은 행동"이 발생했습니다. 프로그래밍이 발전하면서 도구들도 발전했습니다 — 많은 언어에서는 예외를 다루고 있지만, 하드웨어 제약과 C++의 뿌리 때문에 아두이노 세계에서는 여전히 '먼저 검사하기' 스타일이 주를 이룹니다.

아두이노 프로그래밍에서는 에러 처리를 위해 `if-else` 문을 자주 보게 됩니다. 하지만 대안도 있습니다: 조건 실패 시 실행을 중지하는 `assert` 함수를 사용하거나 하드웨어 설정 자체 내에 안전장치를 설계하는 것입니다.

에러 처리를 구현할 때, 프로그램을 중지시키는 것과 기본값이나 안전 상태로 계속 실행을 허용하는 것의 영향을 고려해야 합니다. 양보가 필요하며, 올바른 선택은 중단의 잠재적 해악과 잘못된 작동 사이의 균형에 따라 달라질 수 있습니다.

## 참고하십시오

이들을 통해 에러 감지 및 처리에 대해 더 공부해 보세요:

- 아두이노 언어 참조: https://www.arduino.cc/reference/en/
- 임베디드 아티스트리의 에러 처리에 대한 심층적인 고찰: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- C++ 에러 처리: https://en.cppreference.com/w/cpp/error/exception

이 정보를 바탕으로 아두이노 모험에서의 에러의 함정을 피하는 노하우와 자신감을 얻으시길 바랍니다.
