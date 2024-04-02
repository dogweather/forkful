---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:34.822859-07:00
description: "\"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uC778 YAML\uC740 \uBAA8\
  \uB4E0 \uC885\uB958\uC758 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0 \uC0AC\uC6A9\
  \uD560 \uC218 \uC788\uB294 \uC778\uAC04\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uB370\
  \uC774\uD130 \uC9C1\uB82C\uD654 \uD45C\uC900\uC774\uBA70, \uC124\uC815 \uD30C\uC77C\
  \uBD80\uD130 \uB370\uC774\uD130 \uC800\uC7A5\uAE4C\uC9C0 \uB2E4\uC591\uD55C \uC6A9\
  \uB3C4\uB85C \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC124\uC815 \uD30C\uC77C\uC774\uB098 \uC5B8\uC5B4 \uBC0F \uC2DC\uC2A4\uD15C\
  \ \uAC04\uC758 \uB370\uC774\uD130\u2026"
lastmod: '2024-03-13T22:44:55.958977-06:00'
model: gpt-4-0125-preview
summary: "\"YAML Ain't Markup Language\"\uC758 \uC57D\uC790\uC778 YAML\uC740 \uBAA8\
  \uB4E0 \uC885\uB958\uC758 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC5D0 \uC0AC\uC6A9\
  \uD560 \uC218 \uC788\uB294 \uC778\uAC04\uC774 \uC77D\uC744 \uC218 \uC788\uB294 \uB370\
  \uC774\uD130 \uC9C1\uB82C\uD654 \uD45C\uC900\uC774\uBA70, \uC124\uC815 \uD30C\uC77C\
  \uBD80\uD130 \uB370\uC774\uD130 \uC800\uC7A5\uAE4C\uC9C0 \uB2E4\uC591\uD55C \uC6A9\
  \uB3C4\uB85C \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC124\uC815 \uD30C\uC77C\uC774\uB098 \uC5B8\uC5B4 \uBC0F \uC2DC\uC2A4\uD15C\
  \ \uAC04\uC758 \uB370\uC774\uD130\u2026"
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 무엇이며 왜?

"YAML Ain't Markup Language"의 약자인 YAML은 모든 종류의 애플리케이션에 사용할 수 있는 인간이 읽을 수 있는 데이터 직렬화 표준이며, 설정 파일부터 데이터 저장까지 다양한 용도로 사용됩니다. 프로그래머들은 설정 파일이나 언어 및 시스템 간의 데이터 교환을 위한 쉽게 읽고 쓸 수 있는 포맷이 필요할 때 종종 YAML을 사용합니다.

## 방법:

C에서 YAML을 다루려면 표준 C 라이브러리는 YAML 파싱이나 직렬화를 직접 지원하지 않기 때문에 라이브러리가 필요합니다. C용 가장 인기 있는 YAML 라이브러리 중 하나는 `libyaml`이며, YAML을 파싱하고 생성하기 위한 저수준 및 고수준 인터페이스를 제공합니다. 아래는 `libyaml`을 사용하여 단순한 YAML 파일을 파싱하는 방법의 예입니다:

**첫째**, `libyaml` 라이브러리를 설치해야 합니다. Unix와 유사한 시스템에서는 보통 패키지 관리자를 통해 설치할 수 있습니다. 예를 들어, 우분투에서는:

```bash
sudo apt-get install libyaml-dev
```

**다음으로**, `config.yaml`이라는 이름의 간단한 YAML 파일을 고려해봅시다:

```yaml
name: John Doe
age: 29
married: false
```

**여기** C에서 이 YAML 파일을 파싱하는 기본 예제가 있습니다:

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("Failed to initialize YAML parser!\n", stderr);

    if (fh == NULL)
        fputs("Failed to open file!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Value: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

이 간단한 프로그램은 YAML 파일을 열고, YAML 파서를 초기화하며, 파일을 읽어 스칼라 값(이 예제에서는 우리의 간단한 YAML의 필드)을 출력합니다. 이 간단한 예제에서는 오류 검사가 최소한으로 이루어지며, 생산 코드에서는 더 견고해야 합니다.

`config.yaml`을 가지고 프로그램을 실행하면 아래와 같은 출력을 얻습니다:

```plaintext
Value: John Doe
Value: 29
Value: false
```

## 깊이 있게 들여다보기

YAML은 2001년에 처음 출시되었으며 XML이나 JSON 같은 다른 데이터 직렬화 형식보다 더 읽기 쉽고 사용자 친화적이도록 설계되었으며, 설계 철학을 위해 C, Perl, Python 같은 여러 언어에서 빌려왔습니다. 가독성과 인간이 수정하기 쉬움에도 불구하고 들여쓰기에 의존하고 참조와 사용자 정의 유형을 포함한 광범위한 기능 세트로 인해 프로그래밍적으로 파싱하기 복잡할 수 있습니다.

`libyaml`은 C에서 YAML을 파싱하고 생성하기 위한 강력한 저수준 접근을 제공하지만, 장황한 API로 인해 간단한 작업에는 번거로울 수 있습니다. 이러한 이유로, 일부 프로그래머들은 코드 오버헤드가 최소화된 효율적인 파싱이 우선시될 때 C에서 작업할 때 JSON과 같은 다른 데이터 직렬화 형식이나 더 높은 수준의 라이브러리를 사용하는 것을 선호합니다. 그러나 YAML은 설정 파일과 사람이 읽기에 최적인 상황에서 인기 있는 선택이 계속됩니다. TinyYAML과 같은 대안이나 고수준 인터프리터(예: Python이나 Lua의 내장)를 사용하는 것은 특정 애플리케이션을 위한 편의성과 성능 요구 사이의 균형을 제공할 수 있습니다.
