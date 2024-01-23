---
title:                "YAML 다루기"
html_title:           "Arduino: YAML 다루기"
simple_title:         "YAML 다루기"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
YAML은 설정 파일, 메시지 전달 등에 쓰이는 데이터 직렬화 포맷입니다. 가독성이 좋고 이해하기 쉬워서 프로그래머들이 많이 사용합니다.

## How to: (어떻게 하나요?)
```Python
import yaml

# YAML 스트링을 파이썬 데이터 구조로 로드하기
yaml_data = """
language: Python
version: 3.10
dependencies:
  - numpy
  - pandas
"""

# YAML 파싱
data = yaml.safe_load(yaml_data)

# 파싱된 데이터 출력
print(data)
```
출력:
```
{'language': 'Python', 'version': '3.10', 'dependencies': ['numpy', 'pandas']}
```

## Deep Dive (깊이 알아보기)
YAML("YAML Ain't Markup Language")은 의도적으로 보기 좋게 디자인된 데이터 직렬화 언어로 JSON, XML 같은 경쟁 포맷이 있습니다. 파이썬에서는 `PyYAML` 라이브러리를 주로 사용합니다. `safe_load` 함수는 안전하게 데이터를 로드합니다. 보안 문제로 `load` 함수 사용은 권장하지 않습니다.

## See Also (더 보기)
- PyYAML 공식 문서: https://pyyaml.org/
- YAML 공식 사이트: https://yaml.org/
- Python 공식 문서: https://docs.python.org/3/
