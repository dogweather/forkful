---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/printing-debug-output.md"
---

{{< edit_this_page >}}

# 디버그 출력이란 무엇이고 왜 사용해야 하는가?

디버그 출력은 프로그램 실행 중에 진행 상황과 값을 확인하기 위해 출력 화면에 특정 데이터를 출력하는 것을 말합니다. 이는 버그가 발생했을 때 그 원인을 찾거나, 프로그램의 실행 흐름을 이해하는 데 도움이 됩니다.

# 어떻게 사용하는가:

### 일반 출력
```Python
print("Hello, World!")
```
해당 코드를 실행하면 출력 화면에 `"Hello, World!"`가 표시됩니다.

### 디버그 출력
```Python
import logging
logging.basicConfig(level=logging.DEBUG)
logging.debug("This is a debug message")
```
이 코드를 실행하면 출력 화면에 디버그 메시지가 출력됩니다. 

## 깊은 탐구

디버그 출력에 대한 컨셉은 컴퓨터 프로그래밍 초기부터 존재했으며, 이것이 없으면 프로그램의 동작을 파악하기 어렵습니다. 대안으로 `logging` 라이브러리를 사용할 수 있으며, 장점으로는 로그 수준 설정 등 다양한 레벨 조정이 가능하다는 것입니다.

`print()` 함수는 간단하게 사용할 수 있고, 결과를 즉시 확인할 수 있는 장점이 있습니다. 반면에, `logging.debug()`는 설정에 따라 언제 어떤 정보를 출력할지 바꿀 수 있어, 프로그램의 규모가 클수록 더 유용합니다.

## 참고 자료

1. Python 공식 문서: [디버깅 HOWTO](https://docs.python.org/ko/3/howto/logging.html)
2. Python 공식 문서: [로그 레벨 설정](https://docs.python.org/ko/3/howto/logging.html#when-to-use-logging)
3. Python 공식 문서: [로그 메시지 포매팅](https://docs.python.org/ko/3/howto/logging-cookbook.html#formatting-styles)