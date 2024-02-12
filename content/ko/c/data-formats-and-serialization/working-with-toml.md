---
title:                "TOML과 함께 작업하기"
aliases: - /ko/c/working-with-toml.md
date:                  2024-02-03T18:12:32.537528-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML과 함께 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-toml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

TOML(Tom's Obvious, Minimal Language)은 그 명확한 의미론 때문에 읽기 쉬운 설정 파일 형식입니다. 프로그래머들은 이것의 단순성과 인간이 읽을 수 있는 특성 때문에 XML이나 JSON과 같은 형식들보다 특정 상황에서 뛰어난 선택으로 애플리케이션의 설정 파일에 사용합니다.

## 어떻게 사용하는가:

C에서 TOML을 작업하기 위해서는 C 표준 라이브러리에 이 기능을 포함하고 있지 않기 때문에, TOML 파일을 파싱할 수 있는 라이브러리가 필요합니다. 인기 있는 선택은 C99용 경량 TOML 파서인 `tomlc99`입니다. 간단한 TOML 설정 파일을 읽는 빠른 가이드는 다음과 같습니다.

먼저, `tomlc99`가 설치되어 있고 프로젝트에 제대로 연결되어 있는지 확인하세요.

**샘플 TOML 파일 (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**이 파일을 파싱하는 C 코드:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("파일을 열 수 없습니다");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "파일 파싱 오류\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("데이터베이스 서버: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("포트 %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**출력:**
```
데이터베이스 서버: "192.168.1.1"
포트 0: 8001
포트 1: 8001
포트 2: 8002
```

## 심층 분석

TOML은 다른 설정 파일 형식의 한계를 인식한 GitHub의 공동 창립자인 Tom Preston-Werner에 의해 만들어졌습니다. 그것의 목표는 사람과 컴퓨터 모두에게 복잡한 파싱 규칙 없이 이해하고 쓰기 쉬우며 모호하지 않게 하는 것입니다. C 생태계에서 TOML은 Rust의 `serde_toml`이나 Python의 `toml`과 같이 네이티브 지원을 가진 라이브러리가 있는 고급 언어처럼 1등급 시민은 아닙니다. 대신, C 개발자들은 `tomlc99`와 같은 외부 라이브러리에 의존해야 하지만, 이것은 C의 최소주의와 성능에 대한 강조를 고려할 때 전형적입니다.

TOML이 그 명확성에 대해 칭찬을 받고 있지만, 프로젝트의 요구에 맞는 설정 파일 형식을 선택하는 것이 중요합니다. 웹 API와의 상호 작용이나 더 복잡한 구조가 필요한 시나리오에서 JSON이나 심지어 YAML이 더 적합할 수도 있음에도 불구하고, 이들은 복잡도가 증가합니다. TOML은 가독성과 단순성이 최우선인 구성에서 빛나며, 반드시 가장 고급 데이터 구조가 필요한 것은 아닙니다.
