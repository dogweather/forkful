---
title:                "연관 배열 사용하기"
aliases:
- ko/bash/using-associative-arrays.md
date:                  2024-01-30T19:10:15.847024-07:00
model:                 gpt-4-0125-preview
simple_title:         "연관 배열 사용하기"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜 사용하나?

연관 배열은 정수가 아닌 문자열을 인덱스로 사용할 수 있게 해주는 슈퍼 차지된 배열과 같습니다. 프로그래머들은 보다 복잡한 데이터 구조를 위해 사용하며, 연속적인 리스트에 깔끔하게 들어맞지 않는 데이터를 쉽게 다룰 수 있게 합니다.

## 사용 방법:

우선, Bash에서 연관 배열을 선언하세요:

```Bash
declare -A my_array
```

그 다음, 문자열을 키로 사용하여 값으로 채우기 시작할 수 있습니다:

```Bash
my_array["name"]="Linux Journal"
my_array["topic"]="Programming"
```

요소에 접근하려면 키를 사용하세요:

```Bash
echo ${my_array["name"]}  # 출력: Linux Journal
```

키와 값에 대해 반복하는 것 또한 간단합니다:

```Bash
for key in "${!my_array[@]}"; do
    echo "$key: ${my_array[$key]}"
done
```

샘플 출력은 다음과 같이 보일 수 있습니다:

```
name: Linux Journal
topic: Programming
```

요소를 추가하거나 수정하려면, 초기 채우기와 유사하게 키에 값 할당하기만 하면 됩니다:

```Bash
my_array["readers"]="You"
```

그리고 요소를 제거하려면 `unset`을 사용하세요:

```Bash
unset my_array["topic"]
```

## 심층 분석

연관 배열은 Bash 버전 4.0에서 도입되어 상대적으로 최근에 추가된 언어 기능입니다. 도입 전에는 비정수 인덱스 배열을 다루는 것이 번거롭고, 종종 `awk`나 `sed`와 같은 외부 도구나 우회 방법이 필요했습니다.

내부적으로, Bash는 해시 테이블을 사용하여 연관 배열을 구현합니다. 이 구현은 키 조회를 효율적으로 할 수 있게 해주며, 배열 크기와 관계없이 비교적 일정하게 유지되는데, 이는 스크립트 실행 성능에서 중요한 기능입니다.

Bash의 연관 배열은 셸 스크립팅에 많은 힘과 유연성을 가져다주지만, Python이나 JavaScript와 같은 고급 언어의 배열에 비해 다루기가 다소 불편한 등의 제약이 있습니다. 복잡한 데이터 조작 작업의 경우, 작업에 더 적합한 외부 도구나 언어를 고려하는 것이 여전히 가치가 있을 수 있습니다.

하지만, 많은 전형적인 스크립팅 작업의 경우, 연관 배열은 Bash 프로그래머의 도구 모음에 소중한 도구를 제공하여, 숫자 인덱스 대신 의미 있는 문자열 키를 사용함으로써 보다 읽기 쉽고 유지관리가 용이한 스크립트를 가능하게 합니다.
