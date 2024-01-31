---
title:                "연관 배열 사용하기"
date:                  2024-01-30T19:13:04.756435-07:00
model:                 gpt-4-0125-preview
simple_title:         "연관 배열 사용하기"

category:             "C"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

연관 배열, 또는 해시 맵은 키와 값을 쌍으로 저장하고 키를 사용해 데이터를 검색할 수 있는 방법입니다. 특히 대량의 데이터를 다룰 때 리스트에 비해 데이터 접근 속도가 빠르기 때문에 C언어에서 매우 유용합니다.

## 사용 방법:

C에는 다른 언어처럼 연관 배열에 대한 기본 지원이 없지만, 구조체와 일부 라이브러리 함수를 사용해 비슷한 기능을 구현할 수 있습니다. 여기 `uthash` 라이브러리를 사용한 간단한 구현 방법이 있습니다. 이 라이브러리를 프로젝트에 포함시켜야 합니다.

먼저, 키-값 쌍을 저장할 구조체를 정의합니다:

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // 이것이 우리의 키가 됩니다.
    char name[10]; // 이것은 우리 키와 연결된 값입니다.
    UT_hash_handle hh; // 이 구조체를 해시 가능하게 만듭니다.
} person;
```

다음으로, 몇몇 항목을 추가하고 검색해봅시다:

```C
int main() {
    person *my_people = NULL, *s;

    // 항목 추가
    s = (person*)malloc(sizeof(person));
    s->id = 1;
    strcpy(s->name, "Alice");
    HASH_ADD_INT(my_people, id, s);

    // 항목 검색
    int user_id = 1;
    HASH_FIND_INT(my_people, &user_id, s);
    if (s) {
        printf("Found: %s\n", s->name);
    }
    
    return 0;
}
```

샘플 출력 결과는:

```
Found: Alice
```

사용한 메모리를 해제하고, 메모리 누수를 방지하기 위해 작업을 마친 후에는 해시 테이블을 해제하는 것을 잊지 마세요.

## 심도 있는 탐구

연관 배열이 C에 기본적으로 포함되어 있지 않지만, `uthash` 같은 라이브러리는 이 기능을 사용하는 꽤 직관적인 방법을 제공합니다. 역사적으로 C 개발자들은 이러한 데이터 구조를 직접 구현해야 했으며, 언어에 처음 접하는 사람들에게는 다양하고 종종 복잡한 구현이 이루어지곤 했습니다.

연관 배열을 C에서 사용할 때의 효율성은 해시 함수가 값들을 테이블에 충돌을 최소화하며 얼마나 잘 분산시키는지에 크게 의존합니다. `uthash` 같은 라이브러리는 사용의 용이성과 성능 사이에 좋은 균형을 제공하지만, 성능이 중요한 애플리케이션에서는 자체 해시 테이블을 조정하거나 구현할 수도 있습니다.

최대한의 효율성이 필요한 애플리케이션의 경우, 대안적인 데이터 구조나 연관 배열에 대한 기본 지원이 있는 다른 프로그래밍 언어가 더 나은 선택일 수 있습니다. 그러나 많은 상황에서, 특히 이미 C 환경 내에서 작업하고 있는 경우, `uthash`와 같은 라이브러리를 사용하는 것은 성능과 편의성 사이에 실용적인 균형을 제공합니다.
