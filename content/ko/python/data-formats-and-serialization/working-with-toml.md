---
date: 2024-01-26 04:25:41.481485-07:00
description: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC57D\uC790\uB85C\
  , JSON\uC774\uB098 YAML\uACFC \uC720\uC0AC\uD55C \uB370\uC774\uD130 \uC9C1\uB82C\
  \uD654 \uD615\uC2DD\uC774\uC9C0\uB9CC \uAC04\uACB0\uD568\uACFC \uAC00\uB3C5\uC131\
  \uC744 \uBAA9\uD45C\uB85C \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 TOML\uC744 \uAD6C\uC131 \uD30C\uC77C\uB85C \uC0AC\uC6A9\uD558\uB294\uB370\
  , \uC774\uB294 \uC791\uC131\uD558\uAE30 \uC27D\uACE0 \uC774\uD574\uD558\uAE30 \uC26C\
  \uC6B0\uBA70, \uD30C\uC774\uC36C\uACFC \uAC19\uC740\u2026"
lastmod: '2024-03-13T22:44:54.633602-06:00'
model: gpt-4-0125-preview
summary: "TOML\uC740 Tom's Obvious, Minimal Language\uC758 \uC57D\uC790\uB85C, JSON\uC774\
  \uB098 YAML\uACFC \uC720\uC0AC\uD55C \uB370\uC774\uD130 \uC9C1\uB82C\uD654 \uD615\
  \uC2DD\uC774\uC9C0\uB9CC \uAC04\uACB0\uD568\uACFC \uAC00\uB3C5\uC131\uC744 \uBAA9\
  \uD45C\uB85C \uD569\uB2C8\uB2E4."
title: "\uD504\uB85C\uADF8\uB798\uBA38\uB97C \uC704\uD55C TOML \uB2E4\uB8E8\uAE30"
weight: 39
---

## 방법:
시작하기 전에, `pip install toml`을 사용하여 `toml` 패키지를 설치합시다. TOML 파일을 파싱해 봅시다:

```python
import toml

# 문자열로 된 예시 TOML 내용
toml_string = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # 첫 번째 등급 날짜

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# TOML 문자열 파싱
parsed_toml = toml.loads(toml_string)

# 데이터 접근
print(parsed_toml['owner']['name'])  # 출력: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # 출력: [8001, 8001, 8002]
```

## 심층 탐구
TOML은 GitHub의 창립자 중 한 명인 Tom Preston-Werner에 의해 더 사용자 친화적인 구성 파일 형식으로 만들어졌습니다. 해시 테이블로 모호하지 않게 매핑되도록 설계되었고 기계가 쉽게 파싱할 수 있습니다.

JSON과 비교할 때, TOML은 구성 파일에서 더 읽기 쉽고 주석을 지원합니다. 또 다른 대안인 YAML은 더욱 간결할 수 있지만, 들여쓰기에 의존하고 탭이 허용되지 않는 것과 같은 미묘한 문제들로 인해 사람들을 혼란스럽게 할 수 있습니다.

실행 세부 사항과 관련하여, TOML 값은 문자열, 정수, 부동소수점, 불린, 날짜시간, 배열, 테이블을 포함한 타입이 있습니다. 모든 것은 대소문자를 구별합니다. 또한, TOML은 여러 줄 문자열을 지원하고, 최신 버전에서는 이질적으로 타입이 지정된 배열도 허용합니다.

파이썬은 `toml` 라이브러리를 사용하는데, API 측면에서 JSON 및 YAML 라이브러리와 유사합니다. 파일이나 문자열에서 TOML을 읽기 위해 `toml.load` 및 `toml.loads`를 사용하고, 이를 작성하기 위해 `toml.dump` 및 `toml.dumps`를 사용합니다.

## 참고
- 공식 TOML GitHub 저장소, 사양용: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `toml` 파이썬 라이브러리 문서: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- TOML의 실제 예: 러스트(Rust)의 패키지 관리자 `cargo`나 파이썬 패키징 도구 `poetry`의 구성 파일.
