---
date: 2024-01-26 04:19:54.673101-07:00
description: "TOML (Tom's Obvious, Minimal Language)\uC740 \uBA85\uD655\uD55C \uC758\
  \uBBF8\uB860\uC73C\uB85C \uC778\uD574 \uC77D\uAE30 \uC26C\uC6B4 \uB370\uC774\uD130\
  \ \uC9C1\uB82C\uD654 \uD3EC\uB9F7\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uC778\uAC04\uC758 \uAC00\uB3C5\uC131\uACFC \uAE30\uACC4\uC758 \uD30C\
  \uC2F1 \uAC00\uB2A5\uC131 \uC0AC\uC774\uC758 \uADE0\uD615\uC744 \uB9DE\uCD94\uAE30\
  \ \uB54C\uBB38\uC5D0 \uC124\uC815 \uD30C\uC77C\uC5D0 TOML\uC744 \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.704312-06:00'
model: gpt-4-0125-preview
summary: "TOML (Tom's Obvious, Minimal Language)\uC740 \uBA85\uD655\uD55C \uC758\uBBF8\
  \uB860\uC73C\uB85C \uC778\uD574 \uC77D\uAE30 \uC26C\uC6B4 \uB370\uC774\uD130 \uC9C1\
  \uB82C\uD654 \uD3EC\uB9F7\uC785\uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 무엇인가 & 왜인가?
TOML (Tom's Obvious, Minimal Language)은 명확한 의미론으로 인해 읽기 쉬운 데이터 직렬화 포맷입니다. 프로그래머들은 인간의 가독성과 기계의 파싱 가능성 사이의 균형을 맞추기 때문에 설정 파일에 TOML을 사용합니다.

## 어떻게 사용하는가:
C++에서 TOML을 작업하려면 `toml++`과 같은 라이브러리가 필요합니다. 다음은 빠른 시작 안내입니다:

```C++
#include <toml++/toml.h>
#include <iostream>
#include <fstream>

int main() {
    // 파일에서 TOML 파싱
    std::ifstream ifs("config.toml");
    auto config = toml::parse(ifs);

    // 값에 접근하기
    std::string title = config["title"].value_or("Untitled");
    std::cout << "제목: " << title << '\n';

    // TOML 수정 및 저장
    config["title"] = "새 제목";
    std::ofstream ofs("config.toml");
    ofs << config;
}
```

샘플 `config.toml`:
```toml
title = "예제"
```

샘플 출력:
```plaintext
제목: 예제
```

## 심층 분석
TOML은 2013년 Tom Preston-Werner에 의해 YAML과 JSON에 대한 대안으로 만들어졌습니다. 주로 설정 파일을 위해 간단하고 명시적인 것이 특징입니다. JSON과 다르게 TOML은 모호하지 않게, 즉 문서가 파싱되는 방식이 결정적이라는 것에 중점을 둡니다.

TOML의 대안으로는 YAML이 있는데, YAML은 허용되는 것이 더 많으나 가끔 예측 가능성의 비용으로 이어지기도 합니다. 또 다른 대안인 JSON은 구조가 꽤 엄격하지만, 주석 부족과 괄호 중심의 문법으로 인해 구성에 있어 인간 친화적이지 않습니다.

구현면에서 `toml++`은 최신 TOML 사양과 호환되는 헤더 전용 C++17 라이브러리입니다. DOM과 유사한 인터페이스를 제공하여 TOML 데이터를 탐색하고 조작할 수 있게 해줍니다. 이를 통해 프로젝트에 쉽게 통합할 수 있습니다. 라이브러리는 파싱, 유효성 검사, 및 출력 생성을 관리하여, C++ 타입을 사용하여 TOML 데이터를 가져오고 설정할 수 있게 해줍니다.

## 참고자료
- TOML GitHub 저장소: https://github.com/toml-lang/toml
- TOML용 C++ 라이브러리인 `toml++`: https://github.com/marzer/tomlplusplus
- 포맷의 자세한 설명을 담은 공식 TOML 문서: https://toml.io/en/
