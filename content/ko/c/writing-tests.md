---
title:                "테스트 작성하기"
html_title:           "Arduino: 테스트 작성하기"
simple_title:         "테스트 작성하기"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

테스트 작성은 코드가 의도한 대로 작동하는지 확인하는 과정입니다. 이를 통해 버그를 줄이고 안정성을 높이며 확신을 가지고 코드를 개선할 수 있습니다.

## How to: (방법)

아래에 간단한 C 프로그램과 이에 대한 테스트 코드 예시를 드립니다. 

```C
#include <assert.h>

// 기능: 두 정수의 합을 계산한다.
int add(int a, int b) {
    return a + b;
}

// 테스트 케이스: add 함수 검증
void test_add() {
    assert(add(2, 2) == 4);
    assert(add(-1, 1) == 0);
    assert(add(0, 0) == 0);
    // 추가 테스트 케이스를 계속 추가할 수 있습니다.
}

int main() {
    test_add();
    printf("모든 테스트 통과!\n");
    return 0;
}
```

출력 결과:

```
모든 테스트 통과!
```

## Deep Dive (심층 분석)

테스트 코드는 소프트웨어 개발 초기인 1950년대부터 존재해 왔지만, 2000년대에 들어서야 테스트 주도 개발(TDD) 같은 체계적인 접근법이 보편화되었습니다. 대안으로는 수동 테스트, 통합 테스트, 시스템 테스트 등이 있습니다. C 언어에서는 assert 매크로를 사용한 간단한 테스트부터, CUnit, Check, cmocka 같은 전문 테스트 프레임워크를 사용할 수 있습니다.

## See Also (관련 자료)

- CUnit 공식 웹사이트: http://cunit.sourceforge.net/
- Check 프로젝트: https://libcheck.github.io/check/
- cmocka 프로젝트: https://cmocka.org/
- 테스트 주도 개발(TDD)에 대한 자세한 정보: https://en.wikipedia.org/wiki/Test-driven_development