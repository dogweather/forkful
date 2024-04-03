---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:40.294888-07:00
description: "\uBC29\uBC95: \uC5C4\uBC00\uD788 \uB9D0\uD574\uC11C, \uC544\uB450\uC774\
  \uB178\uB294 \uC0C1\uC704 \uB808\uBCA8 \uC5B8\uC5B4\uC5D0\uC11C \uCC3E\uC744 \uC218\
  \ \uC788\uB294 \uC5F0\uAD00 \uBC30\uC5F4\uC5D0 \uB300\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\
  \uC774 \uC5C6\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098, \uAC71\uC815\uD558\uC9C0 \uB9C8\
  \uC138\uC694. \uAD6C\uC870\uCCB4\uC640 \uBC30\uC5F4\uC744 \uC0AC\uC6A9\uD558\uC5EC\
  \ \uC774 \uAE30\uB2A5\uC744 \uBAA8\uBC29\uD560 \uC218 \uC788\uB294 \uB611\uB611\uD55C\
  \ \uBC29\uBC95\uC774 \uC788\uC2B5\uB2C8\uB2E4. \uC5EC\uAE30 \uB2E4\uB978 \uB3C4\uC2DC\
  \uB4E4\uC758 \uC628\uB3C4\uB97C \uC800\uC7A5\uD558\uACE0 \uC811\uADFC\uD558\uAE30\
  \ \uC704\uD55C \uAE30\uBCF8\uC801\uC778\u2026"
lastmod: '2024-03-13T22:44:55.598385-06:00'
model: gpt-4-0125-preview
summary: "\uC5C4\uBC00\uD788 \uB9D0\uD574\uC11C, \uC544\uB450\uC774\uB178\uB294 \uC0C1\
  \uC704 \uB808\uBCA8 \uC5B8\uC5B4\uC5D0\uC11C \uCC3E\uC744 \uC218 \uC788\uB294 \uC5F0\
  \uAD00 \uBC30\uC5F4\uC5D0 \uB300\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\uC774 \uC5C6\uC2B5\
  \uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 방법:
엄밀히 말해서, 아두이노는 상위 레벨 언어에서 찾을 수 있는 연관 배열에 대한 내장 지원이 없습니다. 그러나, 걱정하지 마세요. 구조체와 배열을 사용하여 이 기능을 모방할 수 있는 똑똑한 방법이 있습니다. 여기 다른 도시들의 온도를 저장하고 접근하기 위한 기본적인 "연관 배열"을 생성하는 간단한 예시가 있습니다.

먼저, 도시(키)와 그 온도(값)를 저장할 구조체를 정의하세요:

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

다음으로, `CityTemperature` 객체의 배열을 초기화하세요:

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

특정 도시의 온도를 접근하고 표시하는 방법은 다음과 같습니다:

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("Los Angeles의 온도는: ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // 여기에는 현재 아무것도 없습니다.
}
```

이 코드를 실행하면 다음과 같은 출력을 얻을 수 있습니다:

```
Los Angeles의 온도는: 22.0
```

## 심층 탐구
역사적으로, C와 C++ 같은 프로그래밍 언어들(아두이노 구문이 파생된 언어들)은 내장 연관 배열이 없어 위와 같은 우회 방법을 써야 했습니다. 이 접근법은 상대적으로 간단하지만, 데이터 크기가 증가함에 따라 O(n) 조회 시간 때문에 규모가 커지면 잘 확장되지 않습니다.

파이썬과 같은 언어는 사전을 제공하고, 자바스크립트에는 이 목적으로 객체가 있으며, 둘 다 키-값 쌍을 관리하는 데 훨씬 더 효율적입니다. 아두이노에서 성능과 효율성이 중요해지면, 개발자들은 라이브러리를 통해 구현된 해시 테이블 같은 더 전문화된 데이터 구조를 선택할 수 있습니다.

아두이노가 기본적으로 연관 배열을 지원하지 않음에도 불구하고, 커뮤니티는 `HashMap`과 같은 라이브러리를 개발하여 프로젝트에 추가함으로써 DIY 접근법보다 더 나은 성능으로 유사한 기능을 제공할 수 있습니다. 이러한 라이브러리는 일반적으로 특히 더 복잡한 프로젝트에 대해 연관 배열을 관리하는 더 우아하고 효율적인 수단을 제공합니다.
