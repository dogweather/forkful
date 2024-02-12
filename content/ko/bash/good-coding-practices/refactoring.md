---
title:                "리팩토링"
aliases:
- /ko/bash/refactoring.md
date:                  2024-01-26T01:16:42.939538-07:00
model:                 gpt-4-0125-preview
simple_title:         "리팩토링"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/refactoring.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
리팩토링은 기존의 컴퓨터 코드를 그 외부 동작을 변경하지 않고 재구조화하는 과정입니다. 이는 복잡성을 줄이고, 유지보수성을 개선함으로써 코드베이스를 건강하고, 현재 및 미래의 개발자들이 이해하기 쉽게 만들기 위한 필수적인 실천입니다.

## 방법:
간단한 Bash 스크립트가 리팩토링이 필요한 상황을 생각해 봅시다. 이 스크립트는 중복된 코드가 있고, 따라가기가 어렵습니다:

```Bash
#!/bin/bash
echo "파일 이름을 입력하세요:"
read filename
if [ -f "$filename" ]; then
    echo "파일이 존재합니다."
    count=$(grep -c "foo" "$filename")
    echo "'foo' 단어가 $count 번 나타납니다."
else
    echo "파일이 존재하지 않습니다."
fi
```

명확성과 재사용성을 위해 리팩토링하는 것은 함수를 도입하고, 오류를 좀 더 우아하게 처리하는 것을 포함할 수 있습니다:

```Bash
#!/bin/bash

function file_exists() {
    [[ -f "$1" ]]
}

function count_occurrences() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "파일 이름을 입력하세요:"
    read -r filename
    echo "검색할 단어를 입력하세요:"
    read -r word

    if file_exists "$filename"; then
        count=$(count_occurrences "$word" "$filename")
        echo "단어 $word가 $count 번 나타납니다."
    else
        echo "파일이 존재하지 않습니다." >&2
        exit 1
    fi
}

main "$@"
```

리팩토링된 버전은 가독성을 향상시키고 재사용 가능성을 활성화하기 위해 함수를 사용합니다.

## 심층 분석:
리팩토링은 Bash나 심지어 고급 프로그래밍 언어와 함께 시작된 개념이 아니며, 프로그래밍 자체만큼 오래된 개념입니다. 이 용어는 1999년 마틴 파울러의 "리팩토링: 기존 코드의 설계 개선"이라는 책에서 공식화되었으며, 주로 객체 지향 언어에 중점을 두었습니다.

Bash 스크립팅 컨텍스트에서, 리팩토링은 종종 긴 스크립트를 함수로 분해하고, 반복을 루프나 조건문으로 줄이며, 파일 이름에 공백을 처리하지 못하는 것과 같은 일반적인 함정을 피하는 것을 의미합니다. 복잡한 작업을 위해 더 나은 데이터 구조와 오류 처리를 제공하는 파이썬이나 펄과 같이, 복잡해진 스크립트에 대한 Bash의 대안도 있습니다.

Bash 특화 리팩토링은 변수를 인용하는 것, `[ ]`보다 `[[ ]]`을 테스트에 사용하는 것, 강력한 출력을 위해 `echo`보다 `printf`를 선호하는 것과 같은 최고의 실천에 따르는 것에 더 관련이 있습니다. 구현 세부 사항은 대부분 스타일 가이드를 준수하고 `shellcheck`과 같은 정적 분석 도구를 사용하여 흔한 실수를 찾는 데 주로 관련이 있습니다.

## 참고 자료:
- [구글의 쉘 스타일 가이드](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, 쉘 스크립트를 위한 정적 분석 도구](https://www.shellcheck.net/)
- [커맨드 라인의 예술](https://github.com/jlevy/the-art-of-command-line)
