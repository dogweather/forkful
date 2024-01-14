---
title:                "C: yaml 과 함께 작업하기"
simple_title:         "yaml 과 함께 작업하기"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-yaml.md"
---

{{< edit_this_page >}}

# 왜?

YAML은 인간이 쉽게 읽고 작성할 수 있는 데이터 직렬화 언어로, C 프로그래밍에서 유용하게 사용될 수 있습니다.

## 사용 방법

YAML을 C 프로그래밍에서 사용하는 방법에 대해 알아보겠습니다. 먼저 YAML 라이브러리를 다운로드해야 합니다. 그 다음 YAML 파일을 읽을 때는 다음과 같은 코드를 사용합니다:

```
yaml_parser_t parser;
yaml_event_t event;

// YAML 파일 열기
FILE *file = fopen("example.yaml", "rb");

// 파일 파싱 준비
yaml_parser_initialize(&parser);
yaml_parser_set_input_file(&parser, file);

// 이벤트 반복 처리
do {

    // 다음 이벤트 읽기
    if (!yaml_parser_parse(&parser, &event)) {
        printf("파싱 에러 %d\n", parser.error);
        exit(EXIT_FAILURE);
    }

    // 이벤트 특정 처리
    switch (event.type) {
        case YAML_SCALAR_EVENT:
            printf("스칼라 이벤트 발견 (%s)\n", event.data.scalar.value);
            break;
        // TODO: 다른 이벤트 타입도 처리
        default:
            printf("이벤트 타입 (%d) 를 처리하지 못했습니다\n", event.type);
    }

    // 이벤트 소거
    yaml_event_delete(&event);

// YAML 문서의 끝에 도달할 때까지
} while (event.type != YAML_STREAM_END_EVENT);

// 파일 닫기
fclose(file);

// 파서 나감
yaml_parser_delete(&parser);
```

YAML 파일을 쓸 때는 비슷한 방법으로 사용할 수 있습니다:

```
// YAML 초기화
yaml_emitter_t emitter;
yaml_event_t event;

// 출력 파일 열기
FILE *file = fopen("example.yaml", "wb");

// 출력 파일 설정
yaml_emitter_initialize(&emitter);
yaml_emitter_set_output_file(&emitter, file);

// 출력 시작
if (!yaml_emitter_dump(&emitter, &event)) {
    printf("등록 에러 %d\n", emitter.error);
    exit(EXIT_FAILURE);
}

// YAML 문서 생성
event.type = YAML_DOCUMENT_START_EVENT;
if (!yaml_emitter_dump(&emitter, &event)) {
    printf("등록 에러 %d\n", emitter.error);
    exit(EXIT_FAILURE);
}

// 스칼라 값을 기록하고 끝냄
event.type = YAML_SCALAR_EVENT;
event.data.scalar.value = (yaml_char_t *)"Hello, World!";
if (!yaml_emitter_dump(&emitter, &event)) {
    printf("등록 에러 %d\n", emitter.error);
    exit(EXIT_FAILURE);
}

// YAML 문서 끝냄
event.type = YAML_DOCUMENT_END_EVENT;
if (!yaml_emitter_dump(&emitter, &event)) {
    printf("등록 에러 %d\n", emitter.error);
    exit(EXIT_FAILURE);
}

// 이벤트 소거
yaml_event_delete(&event);

// YAML 종료
yaml_emitter_close(&emitter);

// 파일 닫기
fclose(file);
```

위 코드는 YAML 문서를 읽어 "스칼라 이벤트 발견 (Hello, World!)"가 출력되며, YAML 파일을 쓰면 해당 문자열이 포함된 파일이 생성됩니다.

## 깊숙한 탐구

YAML은 인간이 쉽게 읽고 작성할 수 있도록 설계되었습니다. 그래서 C 프로그래밍에서 YAML을 사용함으로써 유지보수가 용이해질 수 있습니다. 만약 스칼라 값들을 처리하는데 어려움이 있다면, [YAML 공식 문서](https://yaml.org/)를 참조하는 것이 도움이 될 것입니다.

# 참고 목록

- [YAML 공식 문서](https://yaml.org/)
- [YAML 라이브러리 다운로드](https://github.com/yaml/libyaml)