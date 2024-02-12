---
title:                "표준 에러에 쓰기"
aliases: - /ko/python/writing-to-standard-error.md
date:                  2024-02-03T19:34:33.299970-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 에러에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
파이썬에서 표준 오류로 쓰기는 프로그램의 오류 메시지나 진단을 표준 출력(`stdout`)과 별개인 오류 스트림(`stderr`)으로 리디렉션하는 것입니다. 프로그래머들은 일반 프로그램 출력과 오류 메시지를 구분하기 위해 이 작업을 수행하며, 디버깅과 로그 분석을 용이하게 합니다.

## 방법:
### `sys.stderr` 사용하기
파이썬의 내장 `sys` 모듈은 `stderr`로 명시적으로 쓰기를 허용합니다. 이 접근 방식은 단순한 오류 메시지나 진단에 직관적입니다.

```python
import sys

sys.stderr.write('Error: Something went wrong.\n')
```
표준 오류로의 샘플 출력:
```
Error: Something went wrong.
```

### `print` 함수 사용하기
파이썬의 `print` 함수는 `file` 매개변수를 지정함으로써 출력을 `stderr`로 리디렉션할 수 있습니다. 이 방법은 `print`의 사용자 친화성을 활용하면서 오류 메시지를 처리할 때 유용합니다.
```python
from sys import stderr

print('Error: Failure in module.', file=stderr)
```
표준 오류로의 샘플 출력:
```
Error: Failure in module.
```

### `logging` 모듈 사용하기
보다 포괄적인 해결책으로, 파이썬의 `logging` 모듈은 `stderr`로 메시지를 보낼 수 있는 것과 함께, 파일로 쓰기나 메시지 형식을 사용자 지정하는 등의 기능을 제공합니다. 이 방법은 다양한 수준의 로깅, 메시지 포맷팅, 또는 출력 목적지가 필요한 애플리케이션에 가장 적합합니다.
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Error: Database connection failed.')
```
표준 오류로의 샘플 출력:
```
ERROR:__main__:Error: Database connection failed.
```

### 타사 라이브러리: `loguru`
`loguru`는 파이썬 애플리케이션에서 로깅을 간소화하는 데 도움을 주는 인기 있는 타사 라이브러리입니다. 이 라이브러리는 오류를 자동으로 `stderr`로 리디렉션하는 것을 포함하여 다양한 기능을 제공합니다.

`loguru`를 사용하려면 먼저 pip를 통해 설치하세요:
```shell
pip install loguru
```

그런 다음, 다음과 같이 파이썬 스크립트에 통합하세요:
```python
from loguru import logger

logger.error('Error: Failed to open file.')
```
표준 오류로의 샘플 출력:
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - Error: Failed to open file.
```
