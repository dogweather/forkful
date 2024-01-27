---
title:                "JSON 다루기"
date:                  2024-01-19
html_title:           "Arduino: JSON 다루기"
simple_title:         "JSON 다루기"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-json.md"
---

{{< edit_this_page >}}

# JSON과 작업하기: C 프로그래밍 입문

## 무엇이며 왜 사용하는가?
JSON은 JavaScript Object Notation의 약자로, 데이터를 저장하고 교환하기 위한 경량의 데이터 형식입니다. 개발자들은 JSON을 이용하여 서로 다른 시스템 간에 텍스트 기반의 구조화된 데이터를 쉽고 빠르게 전송하기 위해 사용합니다.

## 사용 방법:
C언어에서 JSON을 다루기 위해서는 보통 외부 라이브러리를 사용해야 합니다. 여기서는 `jansson` 라이브러리의 사용 예시를 소개합니다.

```C
#include <jansson.h>

int main() {
    // JSON 오브젝트 생성
    json_t *root = json_object();
    
    // JSON에 key-value 쌍 추가
    json_object_set_new(root, "name", json_string("김철수"));
    json_object_set_new(root, "age", json_integer(25));
    
    // JSON 오브젝트를 문자열로 변환하여 출력
    char *json_string_data = json_dumps(root, JSON_INDENT(2));
    printf("%s\n", json_string_data);
    
    // 메모리 해제
    free(json_string_data);
    json_decref(root);
    
    return 0;
}
```
출력:
```
{
  "name": "김철수",
  "age": 25
}
```

## 깊이 알아보기:
JSON은 2001년 더글라스 크록포드에 의해 발명되었으며, XML의 복잡함을 대체하기 위한 간단하고 효율적인 포맷으로 널리 사용되게 되었습니다. JSON을 처리하는 데 있어서 C언어는 표준 라이브러리에서 직접 지원하지 않습니다. 그래서 `jansson`, `cJSON`, `json-c` 등의 라이브러리를 사용하여 구현합니다. 각 라이브러리는 다르나 기본적인 사용 방법은 비슷합니다. 

## 관련 자료:
- Jansson 라이브러리 문서: http://www.digip.org/jansson/
- JSON 공식 웹사이트: https://www.json.org/json-en.html
- cJSON 라이브러리 GitHub 페이지: https://github.com/DaveGamble/cJSON
- JSON-C 라이브러리 GitHub 페이지: https://github.com/json-c/json-c

이 자료들을 참고하여, 여러분의 C 프로그램에서 JSON을 쉽게 다루는 법을 더 깊이 배워보세요.
