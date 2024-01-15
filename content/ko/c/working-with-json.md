---
title:                "JSON과 함께 작업하기"
html_title:           "C: JSON과 함께 작업하기"
simple_title:         "JSON과 함께 작업하기"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/working-with-json.md"
---

{{< edit_this_page >}}

# 왜: 왜 누군가가 JSON 작업에 참여할까요?

JSON은 현대 프로그래밍에서 매우 중요한 데이터 형식입니다. 다양한 운영 체제, 프로그래밍 언어 및 데이터 형식 간에 데이터 교환을 쉽게하고 일관성 있게 유지하는 데 사용됩니다. 따라서 어떤 프로그래머라도 JSON을 잘 다루는 것은 중요합니다.


## 사용 방법

JSON을 C 프로그래밍 언어에서 사용하는 방법을 살펴보겠습니다. 

#### JSON 라이브러리 포함
먼저 JSON 데이터를 다루기 위해서는 해당 데이터 형식을 다루는 라이브러리를 불러와야 합니다. C 프로그래밍 언어에서는 [json-c](https://github.com/json-c/json-c) 라이브러리를 사용하면 됩니다. 

#### JSON 데이터 생성
```C
#include <stdio.h>
#include <stdlib.h>
#include <json-c/json.h>

int main(void)
{
    // 새로운 JSON 객체 생성
    json_object *new_obj = json_object_new_object();

    // 키와 값을 추가함
    json_object_object_add(new_obj, "name", json_object_new_string("John"));
    json_object_object_add(new_obj, "age", json_object_new_int(25));

    // 새로운 배열 생성
    json_object *new_arr = json_object_new_array();

    // 배열에 값 추가
    json_object_array_add(new_arr, json_object_new_int(1));
    json_object_array_add(new_arr, json_object_new_int(2));
    json_object_array_add(new_arr, json_object_new_int(3));

    // JSON 객체에 배열 추가
    json_object_object_add(new_obj, "numbers", new_arr);

    // 생성된 JSON 객체 출력
    printf("New JSON object: %s\n", json_object_to_json_string(new_obj));

    // 메모리 반환
    json_object_put(new_obj);
    json_object_put(new_arr);

    return 0;
}
```

#### JSON 데이터 읽기
```C
#include <stdio.h>
#include <json-c/json.h>

int main(void)
{
    // JSON 파일 열기
    FILE *fp = fopen("data.json", "r");
    if(!fp) {
        printf("Error opening JSON file.");
        return 1;
    }

    char buffer[1024];

    // JSON 데이터를 읽어서 버퍼에 저장
    fread(buffer, 1024, 1, fp);

    // 버퍼에서 JSON 객체 생성
    json_object *obj = json_tokener_parse(buffer);

    // 객체에서 값을 읽어서 출력
    printf("Name: %s\n", json_object_get_string(json_object_object_get(obj, "name")));
    printf("Age: %d\n", json_object_get_int(json_object_object_get(obj, "age")));

    // 메모리 반환
    json_object_put(obj);
    fclose(fp);

    return 0;
}
```

#### JSON 데이터 수정
```C
#include <stdio.h>
#include <json-c/json.h>

int main(void)
{
    // JSON 파일 열기
    FILE *fp = fopen("data.json", "r+");
    if(!fp) {
        printf("Error opening JSON file.");
        return 1;
    }

    char buffer[1024];

    // JSON 데이터를 읽어서 버퍼에 저장
    fread(buffer, 1024, 1, fp);

    // 버퍼에서 JSON 객체 생성
    json_object *obj = json_tokener_parse(buffer);

    // 객체에서 값을 수정함
    json_object_object_add(obj, "name", json_object_new_string("Jane"));

    // 수정된 JSON 객체 파일에 씀
    fseek(fp, 0, SEEK_SET);  // 파일 위치를 맨 처음으로 이동
    fprintf(fp, "%s", json_object_to_json_string(obj));

    // 메모리 반환
    json_object_put(obj);
    fclose(fp);

    return 0;
}
```

#### JSON 데이터 삭제
```C
#include <stdio.h>
#include <json-c/json.h>

int main(void)
{
    // JSON 파일 열기
    FILE *fp = fopen("data.json", "r+");
    if