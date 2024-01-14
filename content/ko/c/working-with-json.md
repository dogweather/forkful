---
title:                "C: Json 작업하기"
simple_title:         "Json 작업하기"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-json.md"
---

{{< edit_this_page >}}

## 왜

JSON은 프로그래밍에서 매우 중요합니다. 이것은 데이터를 교환하고 저장하는 데 사용되는 가장 인기있는 형식 중 하나입니다. 따라서 C 프로그래밍 언어로 작업할 때 이를 이해하고 작업하는 것이 필수적입니다.

## 어떻게

JSON을 C 프로그래밍 언어에서 작업하는 방법에 대해 알아보겠습니다. 우리는 먼저 JSON 데이터를 읽고 쓰는 방법을 알아볼 것입니다. 그런 다음 예제 코드를 통해 실제로 이를 수행하는 방법을 배우겠습니다.

### 읽기

다음은 파일에서 JSON 데이터를 읽는 방법을 보여주는 코드 예제입니다.

```C
// 필요한 헤더 파일 포함
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <jansson.h>

int main() {

    // 파일 불러오기
    FILE *fp = fopen("data.json", "r");

    // 파일의 크기 확인
    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    // 파일 내용 저장할 버퍼 할당
    char *file_contents = malloc(file_size + 1);

    // 파일 내용 읽기
    fread(file_contents, 1, file_size, fp);
    fclose(fp);
    file_contents[file_size] = '\0';

    // JSON 객체로 변환
    json_t *root;
    json_error_t error;
    root = json_loads(file_contents, 0, &error);

    // JSON 데이터 읽기
    const char *name = json_string_value(json_object_get(root, "name"));
    const int age = json_integer_value(json_object_get(root, "age"));

    // 읽어온 데이터 출력
    printf("이름: %s\n나이: %d\n", name, age);

    // 메모리 할당 해제
    json_decref(root);
    free(file_contents);
    
    return 0;
}
```

위의 예제에서는 파일을 불러와 파일 크기를 확인하고 해당 크기만큼의 버퍼를 할당한 다음 파일 내용을 읽고 이를 JSON 객체로 변환하여 데이터를 읽어오는 방식을 보여줍니다.

### 쓰기

이번에는 JSON 데이터를 파일에 쓰는 방법에 대해 알아보겠습니다. 다음은 예제 코드입니다.

```C
// 필요한 헤더 파일 포함
#include <stdio.h>
#include <jansson.h>

int main() {

    // JSON 객체 생성
    json_t *root = json_object();

    // JSON 데이터 추가
    json_object_set_new(root, "name", json_string("John"));
    json_object_set_new(root, "age", json_integer(25));

    // JSON 데이터 파일에 쓰기
    FILE *fp = fopen("data.json", "w");
    json_dumpf(root, fp, JSON_INDENT(4));
    fclose(fp);

    // 메모리 할당 해제
    json_decref(root);
    
    return 0;
}
```

위의 예제에서는 먼저 JSON 객체를 생성하고 데이터를 추가한 다음 파일에 쓰는 방식을 보여줍니다. 파일을 열어보면 JSON 형식으로 데이터가 저장된 것을 볼 수 있습니다.

## 심층 분석

JSON은 구조화된 데이터를 보관하기 위한 간단하고 가벼운 형식입니다. 그러나 실제로는 많은 정보가 담긴 복잡한 데이터를 다룰 때 많은 이점이 있습니다. 예를 들어, 여러 사용자의 정보를 담고 있는 JSON 파일을 다룰 경우 각 사용자의 데이터를 쉽게 읽어올 수 있습니다. 또한 여러 데이터를 다룰 때 마찬가지로 각각의 데이터를 식별하여 쉽