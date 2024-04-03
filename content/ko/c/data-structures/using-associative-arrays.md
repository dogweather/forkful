---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:52.955909-07:00
description: "\uBC29\uBC95: C\uB294 \uACE0\uAE09 \uC5B8\uC5B4\uC640 \uAC19\uC740 \uB0B4\
  \uC7A5 \uC9C0\uC6D0\uC774 \uC5C6\uAE30 \uB54C\uBB38\uC5D0, \uAD6C\uC870\uCCB4\uC640\
  \ \uD574\uC2F1\uC744 \uC0AC\uC6A9\uD558\uC5EC \uC5F0\uAD00 \uBC30\uC5F4\uC744 \uBAA8\
  \uBC29\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC544\uB798\uB294 \uAD6C\uC870\uCCB4\
  \uC640 \uAC04\uB2E8\uD55C \uD574\uC2F1 \uD568\uC218\uC758 \uC870\uD569\uC744 \uC0AC\
  \uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4 \uD0A4\uB85C \uC815\uC218\uB97C \uC800\uC7A5\
  \uD558\uACE0 \uC811\uADFC\uD558\uAE30 \uC704\uD55C \uC5F0\uAD00 \uBC30\uC5F4\uC744\
  \ \uAD6C\uD604\uD558\uB294 \uB2E8\uC21C \uC608\uC2DC\uC785\uB2C8\uB2E4. \uBA3C\uC800\
  \ \uB2E8\uC77C \uD0A4-\uAC12 \uC30D\uC744\u2026"
lastmod: '2024-03-13T22:44:55.911114-06:00'
model: gpt-4-0125-preview
summary: "C\uB294 \uACE0\uAE09 \uC5B8\uC5B4\uC640 \uAC19\uC740 \uB0B4\uC7A5 \uC9C0\
  \uC6D0\uC774 \uC5C6\uAE30 \uB54C\uBB38\uC5D0, \uAD6C\uC870\uCCB4\uC640 \uD574\uC2F1\
  \uC744 \uC0AC\uC6A9\uD558\uC5EC \uC5F0\uAD00 \uBC30\uC5F4\uC744 \uBAA8\uBC29\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

## 방법:
C는 고급 언어와 같은 내장 지원이 없기 때문에, 구조체와 해싱을 사용하여 연관 배열을 모방할 수 있습니다. 아래는 구조체와 간단한 해싱 함수의 조합을 사용하여 문자열 키로 정수를 저장하고 접근하기 위한 연관 배열을 구현하는 단순 예시입니다.

먼저 단일 키-값 쌍을 표현할 구조체와 연관 배열 자체를 표현할 다른 구조체를 정의합니다:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} KeyValuePair;

typedef struct {
    KeyValuePair* items[TABLE_SIZE];
} AssocArray;

unsigned int hash(char* key) {
    unsigned long int value = 0;
    unsigned int i = 0;
    unsigned int key_len = strlen(key);

    for (; i < key_len; ++i) {
        value = value * 37 + key[i];
    }

    value = value % TABLE_SIZE;

    return value;
}

void initArray(AssocArray* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void insert(AssocArray* array, char* key, int value) {
    unsigned int slot = hash(key);

    KeyValuePair* item = (KeyValuePair*)malloc(sizeof(KeyValuePair));
    item->key = strdup(key);
    item->value = value;

    array->items[slot] = item;
}

int find(AssocArray* array, char* key) {
    unsigned int slot = hash(key);

    if (array->items[slot]) {
        return array->items[slot]->value;
    }
    return -1;
}

int main() {
    AssocArray a;
    initArray(&a);

    insert(&a, "key1", 1);
    insert(&a, "key2", 2);

    printf("%d\n", find(&a, "key1")); // 출력: 1
    printf("%d\n", find(&a, "key2")); // 출력: 2

    return 0;
}
```

이 예시는 연관 배열을 초기화하고, 키-값 쌍을 삽입하고, 키로 값들을 찾는 기본적인 작업들을 보여줍니다. 이 코드는 충돌 처리를 다루지 않으며 교육용으로 의도되었다는 점에 주목하세요.

## 심층 분석
연관 배열의 개념은 C보다 앞서 있었지만, 이 언어의 저수준 특성은 내장된 유형으로 직접 지원하지 않습니다. 이는 데이터 구조와 알고리즘에 대한 더 깊은 이해를 촉진시키며, 키-값 매핑을 효율적으로 하기 위한 해싱 메커니즘을 포함합니다. 많은 C 라이브러리와 프레임워크는 충돌 처리, 동적 크기 조정 및 임의의 키 및 값 유형 지원을 완비한 강력한 구현을 제공하는 `GHashTable`과 같은 연관 배열을 구현하기 위한 더 정교한 접근법을 제공합니다.

내장 지원이 있는 언어에 비해 C에서 수동으로 연관 배열을 구성하는 것은 번거로워 보일 수 있지만, 데이터 구조의 내부 작동에 대한 소중한 통찰을 제공하고, 문제 해결과 최적화에서 프로그래머의 기술을 갈고닦게 합니다. 그러나, 생산 코드나 더 복잡한 애플리케이션의 경우, GLib과 같은 기존 라이브러리를 활용하는 것이 종종 더 실용적이고 시간 효율적인 접근 방식입니다.
