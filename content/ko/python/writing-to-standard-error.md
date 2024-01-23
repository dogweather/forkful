---
title:                "표준 오류로 쓰기"
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
(무엇이며 왜 사용하는가?)
프로그램은 표준 출력(stdout)과 표준 오류(stderr)를 사용해 메시지를 보낸다. stderr는 오류 메시지나 경고를 보낼 때 사용되며, 이는 로그를 정리하거나 문제 해결할 때 유용하다.

## How to:
(어떻게 사용하는가?)
Python에서 sys 모듈은 stderr를 활용할 수 있는 기능을 제공한다. 예제를 보자.

```Python
import sys

# 표준 오류로 메시지 보내기
sys.stderr.write("이것은 오류 메시지입니다.\n")

# 예외를 발생시키고 표준 오류로 출력하기
try:
    raise ValueError("계산 오류 발생")
except ValueError as e:
    sys.stderr.write(f"예외가 발생했습니다: {e}\n")
```

실행 결과:
```
이것은 오류 메시지입니다.
예외가 발생했습니다: 계산 오류 발생
```

## Deep Dive:
(심층 분석)
stderr는 유닉스 시스템에서 1970년대부터 사용되었다. 만약 sys 모듈의 stderr 대신 print 함수를 사용하고 싶으면, `file` 파라미터를 사용하면 된다.

```Python
print("이것은 표준 오류 출력입니다.", file=sys.stderr)
```

`logging` 모듈을 사용하여 로그 레벨에 맞는 메시지를 stderr로 보낼 수도 있다. stderr를 사용하는 것은 프로그램의 정상적인 출력과 오류 메시지를 분리하는 표준 방식이다.

## See Also:
(참고 자료)
- Python 공식 문서의 sys 모듈 설명: https://docs.python.org/3/library/sys.html
- Python 공식 문서의 logging 모듈 사용법: https://docs.python.org/3/howto/logging.html
- Unix 표준 스트림에 대한 설명: https://en.wikipedia.org/wiki/Standard_streams
