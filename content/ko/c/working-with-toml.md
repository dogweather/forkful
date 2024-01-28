---
title:                "프로그래머를 위한 TOML 다루기"
date:                  2024-01-26T04:19:35.768082-07:00
model:                 gpt-4-0125-preview
simple_title:         "프로그래머를 위한 TOML 다루기"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-toml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
TOML은 읽고 쓰기 쉽도록 설계된 데이터 직렬화 언어입니다. 프로그래머들은 그것의 명확성과 인간 친화성 때문에 설정 파일, 간단한 데이터 저장소, 언어 간 데이터 교환을 위해 사용합니다.

## 방법:
C에서 "tomlc99" 라이브러리를 사용하여 TOML 설정 파일을 파싱해보겠습니다. 먼저, 라이브러리를 설치합니다. 그런 다음, `config.toml`을 만듭니다:

```toml
title = "TOML 예제"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

이제 C에서 파싱합니다:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("오류: 설정 파일을 열 수 없습니다\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("오류: %s\n", errbuf);
        return 1;
    }

    printf("제목: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("소유자 이름: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
샘플 출력:
```
제목: "TOML 예제"
소유자 이름: "Tom Preston-Werner"
```

## 심화 학습
TOML은 Tom의 명백한, 최소한의 언어를 의미하며, 2013년 Tom Preston-Werner에 의해 창안되었습니다. XML과 YAML과 같은 형식에 대한 더 간단한 대안으로써, 더 인간이 읽고 쓰기 쉽도록 중점을 둡니다. JSON도 다른 대안이지만, TOML은 인간이 시각적으로 더 쉽게 파싱할 수 있는 구조를 유지하며, 이는 구성 파일에서 그것이 채택된 주요 이유 중 하나입니다.

C에서 TOML을 사용하는 것은 언어가 기본적으로 지원하지 않기 때문에 파서 라이브러리를 선택하는 것을 포함합니다. "tomlc99"와 같은 라이브러리는 C99 호환 가능하며 TOML 텍스트를 디코딩하는 API를 제공합니다. 성능을 고려할 때, C에는 내장된 가비지 컬렉션이 없기 때문에 적절한 오류 처리와 메모리 관리가 중요합니다.

## 참조:
1. TOML 사양: [https://toml.io/en/](https://toml.io/en/)
2. tomlc99 GitHub 저장소: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. 데이터 직렬화 형식 비교: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
