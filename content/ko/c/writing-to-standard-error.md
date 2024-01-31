---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

표준 에러란, 프로그램 실행 중 오류 메시지를 사용자에게 표시하는 출력 스트림입니다. 로그 파일과 분리하여 신속한 디버깅을 위해 사용합니다.

## How to: (사용 방법)

```C
#include <stdio.h>

int main() {
    fprintf(stderr, "에러 발생!\n");
    return 0;
}
```

위 코드는 "에러 발생!"을 표준 에러 스트림에 출력합니다. 콘솔로 확인 가능합니다.

## Deep Dive (심층 분석)

프로그램에는 표준 출력(stdout)와 표준 에러(stderr)가 있습니다. 이는 UNIX 시스템의 초기 설계에서 비롯되었어요. 대안으로는 syslog나 파일 로깅이 있습니다. stderr은 unbuffered이기에 메시지가 즉시 출력됩니다.

## See Also (추가 정보)

- C 표준 라이브러리 문서: https://en.cppreference.com/w/c/io
- UNIX 철학과 I/O 스트림 이해: https://en.wikipedia.org/wiki/Unix_philosophy
