---
title:                "Python: 표준 오류 출력에 쓰는 방법"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜 

표준 에러를 작성하는 것은 디버깅 하고 코드 에러를 추적하는데 매우 중요합니다.

## 어떻게

```Python
import sys

# 표준 에러로 메시지를 작성하는 예제
sys.stderr.write("이것은 표준 에러 메시지입니다.")

# 예외를 발생시키는 예제
try:
  raise Exception("이것은 예외 메시지입니다.")
except Exception as e:
  # 예외 메시지를 표준 에러로 작성
  sys.stderr.write(str(e))
```

출력 예시:
```
이것은 표준 에러 메시지입니다.
이것은 예외 메시지입니다.
```

## 깊이있게 살펴보기

표준 에러는 표준 출력과 달리 프로그램의 진행을 방해하지 않고 디버깅에 매우 유용합니다. 또한 ```try-except``` 구문과 함께 사용되어 예외 처리에도 매우 유용합니다.

## 또한 참고

- [Python 공식 문서 - sys 모듈](https://docs.python.org/3/library/sys.html)
- [Python 공식 문서 - 예외 처리](https://docs.python.org/3/tutorial/errors.html)