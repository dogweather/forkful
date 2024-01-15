---
title:                "yaml로 작업하기"
html_title:           "Arduino: yaml로 작업하기"
simple_title:         "yaml로 작업하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## 왜 YAML을 사용해야 하는가?

YAML은 사람과 컴퓨터 모두에게 읽기 쉽고 이해하기 쉬운 형식으로 데이터를 저장할 수 있는 파일 형식입니다. 이를 이용하면 더 간결하고 유지보수가 용이한 코드를 작성할 수 있습니다. 또한 다른 언어와 호환성이 좋아 다양한 프로그래밍 환경에서 사용할 수 있습니다.

## 어떻게 사용할까요?

YAML을 사용하기 위해서는 먼저 Arduino IDE의 "Library Manager"로 들어가서 YAML 라이브러리를 설치해야 합니다. 이후 코드를 작성할 때는 아래와 같이 라이브러리를 불러와야 합니다.

```Arduino
#include <YAML.h>
```

YAML 파일을 읽어오기 위해서는 `YAML::load` 함수를 사용합니다. 예를 들어, `config.yml` 파일에 저장된 데이터를 읽어오려면 다음과 같이 작성할 수 있습니다.

```Arduino
YAML::Node config = YAML::LoadFile("config.yml");
```

해당 데이터에 접근하려면 `config` 변수를 이용해 키-값 쌍에 접근합니다.

```Arduino
int baudRate = config["baud_rate"].as<int>();
```

## 깊게 파보기

YAML의 세부적인 문법과 기능은 다양하지만, 가장 많이 쓰이는 방법은 키-값 쌍으로 데이터를 저장하는 것입니다. 이 때 키는 문자열이고 값은 문자열, 정수, 실수, 불린 등 여러 가지 형식으로 사용할 수 있습니다. YAML 파일을 작성할 때는 들여쓰기를 통해 계층 구조를 나타낼 수 있으며, 주석도 추가할 수 있습니다.

또한 YAML은 C++과 같은 다른 언어로도 사용할 수 있으며, 커스텀 데이터 타입을 정의하여 YAML에 저장할 수도 있습니다. YAML을 자세히 공부하면 더 다양한 방법으로 코드를 작성할 수 있습니다.

## 관련 자료

- [YAML 공식 문서](http://yaml.org/)
- [YAML 라이브러리 Github 저장소](https://github.com/arduino-libraries/YAML)