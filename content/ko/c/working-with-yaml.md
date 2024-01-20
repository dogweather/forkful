---
title:                "YAML 다루기"
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
YAML은 데이터 직렬화 형식입니다. YAML은 구성, 설정, 메시지 전송에 널리 사용되며 JSON이나 XML보다 읽기 쉽다는 장점이 있습니다. 프로그래머들은 사용자 친화적인 데이터 저장 및 전송을 위해 YAML을 다룹니다.

## How to: (어떻게 하나요?)
C에서 YAML을 다루려면 외부 라이브러리를 사용해야 합니다. 예를 들어, `libyaml`이 있습니다. 아래 예제는 YAML 파일을 읽고 파싱한다.

```C
#include <yaml.h>

int main(int argc, char** argv) {
    FILE *fh = fopen("example.yaml", "r");
    yaml_parser_t parser;
    yaml_event_t event;

    /* Initialize parser */
    if(!yaml_parser_initialize(&parser))
        fputs("Failed to initialize parser!\n", stderr);
    if(fh == NULL)
        fputs("Failed to open file!\n", stderr);
    
    /* Set input file */
    yaml_parser_set_input_file(&parser, fh);

    /* Read the YAML events */
    do {
        if (!yaml_parser_parse(&parser, &event)) {
            printf("Parser error %d\n", parser.error);
            break;
        }
        
        // 처리 로직은 여기에...
        
        if(event.type != YAML_STREAM_END_EVENT)
            yaml_event_delete(&event);
    } while(event.type != YAML_STREAM_END_EVENT);
    
    /* Cleanup */
    yaml_event_delete(&event);
    yaml_parser_delete(&parser);
    fclose(fh);

    return 0;
}
```

이 코드는 `example.yaml` 파일을 열고, YAML 이벤트를 읽어 들입니다.

## Deep Dive (심층 분석)
YAML은 "YAML Ain't Markup Language"(원래는 "Yet Another Markup Language")의 재귀 약어입니다. 2001년에 개발되었으며, 데이터를 계층 구조로 표현할 수 있게 해줍니다. YAML은 JSON과 호환되는데, JSON 문법은 YAML의 부분 집합입니다. `libyaml`는 C 언어로 작성된 YAML 1.1 파서 및 방출기입니다. 대안으로는 `yaml-cpp`, `PyYAML` 등이 있습니다. 성능은 라이브러리마다 차이가 있으므로 사용 사례에 따라 적절한 것을 선택해야 합니다.

## See Also (참고 자료)
- YAML 공식 웹사이트: https://yaml.org
- libyaml 깃허브: https://github.com/yaml/libyaml
- yaml-cpp 깃허브: https://github.com/jbeder/yaml-cpp
- PyYAML: https://pyyaml.org