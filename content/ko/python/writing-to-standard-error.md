---
title:                "Python: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

표준 에러에 대해 작성하는 것은 프로그램의 디버깅을 돕기 위해 중요합니다.

## 하우 투

```Python
try:
    # 코드 실행
except Exception as e:
    # 에러 메시지를 표준 에러에 작성
    sys.stderr.write(str(e))
```

예외 처리를 사용하여 코드를 실행하고, 발생한 예외를 변수로 저장합니다. 그리고 해당 변수를 문자열로 변환하여 표준 에러에 작성하는 방식으로 표준 에러에 대한 메시지를 출력할 수 있습니다.

## 딥 다이브

표준 에러는 프로그램 실행 중에 발생한 예외 및 오류의 상세 정보를 제공합니다. 이를 활용하여 디버깅에 유용하게 사용할 수 있습니다. 또한 표준 에러를 파일로 저장하면 프로그램 실행 이후에도 오류 메시지를 확인할 수 있습니다.

## 연관 항목

- [파이썬 예외 처리](https://www.python.org/dev/peps/pep-0343/)
- [표준 에러와 표준 출력](https://www.geeksforgeeks.org/python-output-error-handling/)
- [디버깅의 기초](https://code.tutsplus.com/ko/tutorials/debugging-for-beginners--net-7135)