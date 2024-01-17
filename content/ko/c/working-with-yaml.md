---
title:                "yaml 작업하기"
html_title:           "C: yaml 작업하기"
simple_title:         "yaml 작업하기"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

YAML 작업은 구조화된 데이터를 저장하고 전달하는 데 사용되는 경량 마크업 언어입니다. 프로그래머들은 YAML을 사용하여 복잡한 데이터를 쉽게 다룰 수 있고, 코드를 더 효율적이고 읽기 쉽게 만들 수 있기 때문에 사용합니다.

## 어떻게:

```C
#include <yaml/yaml.h>

int main() {
  // YAML 데이터 작성하기
  yaml_emitter_t emitter;
  yaml_event_t event;
  
  yaml_emitter_init(&emitter);
  //파일 스트림으로 출력
  yaml_emitter_set_output_file(&emitter, fp);

  // YAML 스트림 시작
  yaml_stream_start_event_initialize(&event, YAML_UTF8_ENCODING);
  yaml_emitter_emit(&emitter, &event);
  yaml_event_delete(&event);

  // YAML 문서 시작
  yaml_document_start_event_initialize(&event, NULL, NULL, NULL, 0);
  yaml_emitter_emit(&emitter, &event);
  yaml_event_delete(&event);

  // 스칼라 데이터 추가
  yaml_scalar_event_initialize(
      &event, NULL, NULL,
      (yaml_char_t *)"key", 3, // "key" ->키로 변환
      1, 1, YAML_PLAIN_SCALAR_STYLE);
  yaml_emitter_emit(&emitter, &event);
  yaml_event_delete(&event);

  // 맵 시작
  yaml_mapping_start_event_initialize(
      &event, NULL, NULL, 1, YAML_BLOCK_MAPPING_STYLE);
  yaml_emitter_emit(&emitter, &event);
  yaml_event_delete(&event);

  // 키-값 쌍 추가
  yaml_scalar_event_initialize(
      &event, NULL, NULL,
      (yaml_char_t *)"value", 5, // "value" ->값으로 변환
      1, 1, YAML_PLAIN_SCALAR_STYLE);
  yaml_emitter_emit(&emitter, &event);
  yaml_event_delete(&event);

  yaml_scalar_event_initialize(
      &event, NULL, NULL,
      (yaml_char_t *)"another_key", 11, // "another_key" -> 다른 키로 변환
      1, 1, YAML_PLAIN_SCALAR_STYLE);
  yaml_emitter_emit(&emitter, &event);
  yaml_event_delete(&event);

  yaml_scalar_event_initialize(
      &event, NULL, NULL,
      (yaml_char_t *)"another_value", 13, // "another_value" -> 다른 값으로 변환
      1, 1, YAML_PLAIN_SCALAR_STYLE);
  yaml_emitter_emit(&emitter, &event);
  yaml_event_delete(&event);

  // 맵 끝
  yaml_mapping_end_event_initialize(&event);
  yaml_emitter_emit(&emitter, &event);
  yaml_event_delete(&event);

  // 문서 끝
  yaml_document_end_event_initialize(&event, 0);
  yaml_emitter_emit(&emitter, &event);
  yaml_event_delete(&event);

  // YAML 스트림 끝
  yaml_stream_end_event_initialize(&event);
  yaml_emitter_emit(&emitter, &event);
  yaml_event_delete(&event);

  // YAML 작업 완료
  yaml_emitter_delete(&emitter);

  return 0;
}
```

출력:

```yaml
--- # key-value 쌍의 맵으로 구성된 YAML 문서
key: value
another_key: another_value
```

## 깊은 연구:

- YAML은 2001년에 처음 발표된 마크업 언어로, XML과 비교하여 더 간단하고 읽기 쉬운 구조를 가지고 있습니다.
- JSON은 YAML의 경량 버전으로, 더 단순한 데이터 구조를 가지고 있습니다.
- YAML 작업은 객체 지향 프로그래밍과 데이터 직렬화를 위해 널리 사용됩니다.

## 참고:

- https://yaml.org/
- https://www.json.org/