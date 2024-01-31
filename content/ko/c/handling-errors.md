---
title:                "오류 처리"
date:                  2024-01-26T00:36:55.453723-07:00
model:                 gpt-4-1106-preview
simple_title:         "오류 처리"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/handling-errors.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
C에서 오류를 처리하는 것은 예상치 못한 상황을 대비하는 것입니다. 프로그램이 문제에 부딪혔을 때 제멋대로 굴러가는 것을 막습니다. 프로그래머들은 이를 통해 실수를 우아하게 처리하고 코드의 신뢰성을 유지합니다.

## 어떻게 할까요?

C에서 이를 어떻게 하는지 살펴봅시다:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("nonexistentfile.txt", "r");
    if (fp == NULL) {
        perror("파일을 여는 데 오류가 발생했습니다");
        return EXIT_FAILURE;
    }
    // 파일로 무언가를 합니다
    fclose(fp);
    return EXIT_SUCCESS;
}
```

파일이 존재하지 않을 때의 샘플 출력:
```
파일을 여는 데 오류가 발생했습니다: 그런 파일이나 디렉토리가 없습니다
```

## 심층 분석

초기 C 언어 시대에는 오류 처리가 기본적이었으며 - 주로 반환 코드와 수동 검사로 이루어졌습니다. 그래서 `errno`, 함수가 실패할 때 업데이트 되는 전역 변수가 생겼습니다. 그러나 이 자체만으로는 스레드 안전성이 보장되지 않아, 더 나은 오류 보고를 위해 `strerror` 및 `perror` 함수가 도입되었습니다.

대안은? 현대의 C는 `errno`에만 국한되지 않습니다. 재앙이 닥쳤을 때 비지역 점프를 위한 setjmp와 longjmp가 있습니다. 일부는 자신들만의 오류 코드를 정의하는 것을 선호하는 반면, 다른 일부는 C++의 예외처럼 구성된 구조를 선택합니다.

구현 세부 사항은 복잡할 수 있습니다. 예를 들어, POSIX 호환 시스템에서는 스레드 로컬 저장소(TLS)의 마법 덕분에 `errno`이 스레드 안전하게 됩니다. 자원이 소중한 임베디드 시스템에서는 소프트웨어를 부풀릴 수 있는 표준 접근 방식보다 맞춤 오류 처리 코드가 선호될 수 있습니다.

## 참고자료

- `errno`에 대한 자세한 탐구: https://en.cppreference.com/w/c/error/errno
- 스레드 안전성을 위해서는 POSIX 스레드와 errno를 참조하세요: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- setjmp와 longjmp에 대한 소개: https://www.cplusplus.com/reference/csetjmp/
- C++에서의 예외 처리를 확인하려면: https://isocpp.org/wiki/faq/exceptions
