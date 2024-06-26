---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:37.998741-07:00
description: "\uC5B4\uB5BB\uAC8C: C\uC5D0\uC11C JSON\uC744 \uB2E4\uB8E8\uB824\uBA74\
  , C\uAC00 JSON\uC744 \uAE30\uBCF8\uC801\uC73C\uB85C \uC9C0\uC6D0\uD558\uC9C0 \uC54A\
  \uAE30 \uB54C\uBB38\uC5D0 `jansson` \uB610\uB294 `json-c`\uC640 \uAC19\uC740 \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC774 \uC77C\uBC18\
  \uC801\uC785\uB2C8\uB2E4. \uC5EC\uAE30\uC11C\uB294 \uC0AC\uC6A9\uC758 \uC6A9\uC774\
  \uC131\uACFC \uD65C\uBC1C\uD55C \uC720\uC9C0 \uAD00\uB9AC\uB85C `jansson`\uC5D0\
  \ \uCD08\uC810\uC744 \uB9DE\uCDA5\uB2C8\uB2E4. \uBA3C\uC800, \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uB97C\u2026"
lastmod: '2024-03-13T22:44:55.960606-06:00'
model: gpt-4-0125-preview
summary: "C\uC5D0\uC11C JSON\uC744 \uB2E4\uB8E8\uB824\uBA74, C\uAC00 JSON\uC744 \uAE30\
  \uBCF8\uC801\uC73C\uB85C \uC9C0\uC6D0\uD558\uC9C0 \uC54A\uAE30 \uB54C\uBB38\uC5D0\
  \ `jansson` \uB610\uB294 `json-c`\uC640 \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC774 \uC77C\uBC18\uC801\uC785\uB2C8\uB2E4\
  ."
title: "JSON \uC791\uC5C5\uD558\uAE30"
weight: 38
---

## 어떻게:
C에서 JSON을 다루려면, C가 JSON을 기본적으로 지원하지 않기 때문에 `jansson` 또는 `json-c`와 같은 라이브러리를 사용하는 것이 일반적입니다. 여기서는 사용의 용이성과 활발한 유지 관리로 `jansson`에 초점을 맞춥니다. 먼저, 라이브러리를 설치하세요(예: Ubuntu에서 패키지 매니저인 `apt`를 사용하여: `sudo apt-get install libjansson-dev`).

JSON 문자열을 파싱하고 내용에 접근하는 것부터 시작해봅시다:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "error: on line %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("이름: %s\n나이: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

출력 예:
```
이름: John Doe
나이: 30
```

다음으로, JSON 객체를 생성하고 출력해봅시다:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

출력 예:
```
{"name": "Jane Doe", "age": 25}
```

이 예시들은 JSON 문자열을 로드하고, 그 값을 언팩하고, 새로운 JSON 객체를 생성한 다음 문자열로 출력하는 기본 사항을 보여줍니다.

## 심층 탐구
웹이 데이터 교환을 위한 주요 포맷으로 JSON을 채택함에 따라 C에서 JSON을 다루어야 할 필요성이 대두되었습니다. JSON의 단순성과 효율성은 C가 처음에 JSON 조작을 직접 지원하지 않았음에도 불구하고 XML을 빠르게 앞지르게 하였습니다. 초기 해결책에는 수동 문자열 조작이 포함되어 있었는데, 이는 오류가 발생하기 쉽고 비효율적이었습니다. `jansson` 및 `json-c` 같은 라이브러리가 이 공백을 메워 JSON 파싱, 구축 및 직렬화를 위한 강력한 API를 제공하게 되었습니다.

`jansson`은 사용의 용이성을 제공하는 반면, `json-c`는 더 넓은 기능 세트를 찾는 이들에게 매력적일 수 있습니다. 그럼에도 불구하고, C++과 같은 언어의 파싱 라이브러리는 보다 복잡한 데이터 구조와 표준 라이브러리 지원 덕분에 더 정교한 추상화를 제공합니다. 그러나 임베디드 시스템에서 또는 기존 C 라이브러리와 인터페이싱할 때와 같이 C가 선호되거나 필요한 언어인 환경에서 작업할 때는 `jansson` 또는 `json-c`를 사용하는 것이 필수적입니다.

C에서 JSON을 다루는 것은 이러한 라이브러리들이 종종 명시적인 할당 해제를 요구하는 동적으로 할당된 객체를 자주 반환한다는 점을 고려할 때, 메모리 관리에 대한 더 깊은 이해를 포함한다는 것도 주목할 가치가 있습니다. 프로그래머들에게 메모리 누수를 방지하는 책임과 편리함 사이의 균형을 맞추는 것은 효율적인 C 코드를 작성하는 데 있어 중요한 측면입니다.
